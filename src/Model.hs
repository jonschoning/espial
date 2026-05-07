{-# LANGUAGE DeriveFunctor #-}

module Model where

import ClassyPrelude.Yesod hiding (Value, exists, groupBy, on, (<=.), (==.), (>=.), (||.))
import qualified ClassyPrelude.Yesod as CP
import qualified Control.Monad.Combinators as PC (between)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Monad.Writer (tell)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as A (parseFail)
import qualified Data.Attoparsec.Text as P
import Data.Char (isSpace)
import Data.Either (fromRight)
import Data.Foldable (foldl, foldl1, sequenceA_)
import qualified Data.Time as TI (ParseTime)
import qualified Data.Time.ISO8601 as TISO (formatISO8601Millis, parseISO8601)
import Database.Esqueleto.Experimental hiding ((<&>))
import Database.Esqueleto.Internal.Internal (unsafeSqlFunction)
import Model.Custom
import Pretty ()
import Types

share
  [mkPersist sqlSettings, mkMigrate "migrateSchema"]
  [persistLowerCase|
User json
  name Text
  passwordHash BCrypt
  apiToken HashedApiKey Maybe
  privateDefault Bool
  archiveDefault Bool
  suggestTags Bool default=True
  privacyLock Bool
  UniqueUserName name
  deriving Show Eq Typeable Ord

Bookmark json
  userId UserId OnDeleteCascade
  slug BmSlug default="(lower(hex(randomblob(6))))"
  href Text
  description Text
  extended Text
  time UTCTime
  shared Bool
  toRead Bool
  selected Bool
  archiveHref Text Maybe
  UniqueUserHref userId href
  UniqueUserSlug userId slug
  deriving Show Eq Typeable Ord

BookmarkTag json
  userId UserId OnDeleteCascade
  tag Text
  bookmarkId BookmarkId OnDeleteCascade
  seq Int
  UniqueUserTagBookmarkId userId tag bookmarkId
  UniqueUserBookmarkIdTagSeq userId bookmarkId tag seq
  deriving Show Eq Typeable Ord

Note json
  userId UserId  OnDeleteCascade
  slug NtSlug default="(lower(hex(randomblob(10))))"
  length Int
  title Text
  text Text
  isMarkdown Bool
  shared Bool default=False
  created UTCTime
  updated UTCTime
  deriving Show Eq Typeable Ord
|]

newtype UTCTimeStr
  = UTCTimeStr {unUTCTimeStr :: UTCTime}
  deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

instance PathPiece UTCTimeStr where
  toPathPiece (UTCTimeStr u) = pack (TISO.formatISO8601Millis u)
  fromPathPiece s = UTCTimeStr <$> TISO.parseISO8601 (unpack s)

newtype UserNameP
  = UserNameP {unUserNameP :: Text}
  deriving (Eq, Show, Read)

newtype TagsP
  = TagsP {unTagsP :: [Text]}
  deriving (Eq, Show, Read)

data SharedP
  = SharedAll
  | SharedPublic
  | SharedPrivate
  deriving (Eq, Show, Read)

data FilterP
  = FilterAll
  | FilterUnread
  | FilterUntagged
  | FilterStarred
  | FilterSingle BmSlug
  deriving (Eq, Show, Read)

newtype UnreadOnly
  = UnreadOnly {unUnreadOnly :: Bool}
  deriving (Eq, Show, Read)

newtype UserAgent
  = UserAgent {unUserAgent :: Text}
  deriving (Eq, Show, Read)

newtype Url
  = Url {unUrl :: Text}
  deriving (Eq, Show, Read)

type Limit = Int64

type Page = Int64

migrateAll :: Migration
migrateAll = migrateSchema >> migrateIndexes

dumpMigration :: DB ()
dumpMigration = printMigration migrateAll

runMigrations :: DB ()
runMigrations = runMigration migrateAll

toMigration :: [Text] -> Migration
toMigration = lift . tell . fmap (False,)

migrateIndexes :: Migration
migrateIndexes =
  toMigration
    [ "CREATE INDEX IF NOT EXISTS idx_bookmark_time ON bookmark (user_id, time DESC)",
      "CREATE INDEX IF NOT EXISTS idx_bookmark_tag_bookmark_id ON bookmark_tag (bookmark_id, id, tag, seq)",
      "CREATE INDEX IF NOT EXISTS idx_note_user_created ON note (user_id, created DESC)"
    ]

sqliteGroupConcat ::
  (PersistField a) =>
  SqlExpr (Value a) ->
  SqlExpr (Value a) ->
  SqlExpr (Value Text)
sqliteGroupConcat expr sep = unsafeSqlFunction "GROUP_CONCAT" [expr, sep]

authenticatePassword :: Text -> Text -> DB (Maybe (Entity User))
authenticatePassword username password = do
  getBy (UniqueUserName username) >>= \case
    Nothing -> pure Nothing
    Just dbuser ->
      if validatePasswordHash (userPasswordHash (entityVal dbuser)) password
        then pure (Just dbuser)
        else pure Nothing

getUserByName :: UserNameP -> DB (Maybe (Entity User))
getUserByName (UserNameP uname) =
  selectFirst [UserName CP.==. uname] []

getApiKeyUser :: ApiKey -> DB (Maybe (Entity User))
getApiKeyUser apiKey =
  selectFirst [UserApiToken CP.==. Just (hashApiKey apiKey)] []

-- returns a list of pair of bookmark with tags merged into a string
bookmarksTagsQuery ::
  Key User ->
  Bool ->
  SharedP ->
  FilterP ->
  [Tag] ->
  Maybe Text ->
  Limit ->
  Page ->
  DB (Int, [(Entity Bookmark, Maybe Text)])
bookmarksTagsQuery userId isowner sharedp filterp tags mquery limit' page =
  (,) -- total count
    <$> fmap
      (sum . fmap unValue)
      ( select $ from (table @Bookmark) >>= \b -> do
          _whereClause b
          pure countRows
      )
    -- paged data
    <*> (fmap . fmap . fmap)
      unValue
      ( select $ from (table @Bookmark) >>= \b -> do
          _whereClause b
          orderBy [desc (b ^. BookmarkTime)]
          limit limit'
          offset ((page - 1) * limit')
          pure
            ( b,
              subSelect $ from (table @BookmarkTag) >>= \t -> do
                where_ (t ^. BookmarkTagBookmarkId ==. b ^. BookmarkId)
                when
                  (not isowner)
                  (where_ (not_ (t ^. BookmarkTagTag `like` val ".%")))
                groupBy (t ^. BookmarkTagBookmarkId)
                orderBy [asc (t ^. BookmarkTagSeq)]
                pure $ sqliteGroupConcat (t ^. BookmarkTagTag) (val " ")
            )
      )
  where
    _whereClause b = do
      where_
        $ foldl
          ( \expr tag ->
              expr
                &&. exists
                  ( -- each tag becomes an exists constraint
                    from (table @BookmarkTag) >>= \t ->
                      where_
                        ( t ^. BookmarkTagBookmarkId ==. b ^. BookmarkId
                            &&. (t ^. BookmarkTagTag `like` val tag)
                        )
                  )
          )
          (b ^. BookmarkUserId ==. val userId)
          tags
      case sharedp of
        SharedAll -> pure ()
        SharedPublic -> where_ (b ^. BookmarkShared ==. val True)
        SharedPrivate -> where_ (b ^. BookmarkShared ==. val False)
      case filterp of
        FilterAll -> pure ()
        FilterUnread -> where_ (b ^. BookmarkToRead ==. val True)
        FilterStarred -> where_ (b ^. BookmarkSelected ==. val True)
        FilterSingle slug -> where_ (b ^. BookmarkSlug ==. val slug)
        FilterUntagged ->
          where_ $ notExists $ from (table @BookmarkTag) >>= \t ->
            where_
              $ t
              ^. BookmarkTagBookmarkId
              ==. b ^. BookmarkId
      -- search
      sequenceA_ (parseSearchQuery (toLikeExpr b) =<< mquery)

    toLikeExpr :: SqlExpr (Entity Bookmark) -> Text -> SqlExpr (Value Bool)
    toLikeExpr b term = fromRight p_allFields (P.parseOnly p_onefield term)
      where
        wild s = (%) ++. val s ++. (%)
        toLikeB field s = b ^. field `like` wild s
        p_allFields =
          toLikeB BookmarkHref term
            ||. toLikeB BookmarkDescription term
            ||. toLikeB BookmarkExtended term
            ||. exists
              ( from (table @BookmarkTag) >>= \t ->
                  where_
                    $ (t ^. BookmarkTagBookmarkId ==. b ^. BookmarkId)
                    &&. (t ^. BookmarkTagTag `like` wild term)
              )
        p_onefield = p_url <|> p_title <|> p_description <|> p_tags <|> p_after <|> p_before
          where
            p_url = "url:" *> fmap (toLikeB BookmarkHref) P.takeText
            p_title = "title:" *> fmap (toLikeB BookmarkDescription) P.takeText
            p_description = "description:" *> fmap (toLikeB BookmarkExtended) P.takeText
            p_tags =
              "tags:"
                *> fmap
                  ( \term' ->
                      exists $ from (table @BookmarkTag) >>= \t ->
                        where_
                          $ (t ^. BookmarkTagBookmarkId ==. b ^. BookmarkId)
                          &&. (t ^. BookmarkTagTag `like` wild term')
                  )
                  P.takeText
            p_after = "after:" *> fmap ((b ^. BookmarkTime >=.) . val) (parseTimeText =<< P.takeText)
            p_before = "before:" *> fmap ((b ^. BookmarkTime <=.) . val) (parseTimeText =<< P.takeText)

-- returns a list of pair of bookmark with tags merged into a string
allUserBookmarks :: Key User -> DB [(Entity Bookmark, Text)]
allUserBookmarks user =
  (fmap . fmap . fmap) (fromMaybe "" . unValue)
    $ select
    $ do
      b <- from (table @Bookmark)
      where_ (b ^. BookmarkUserId ==. val user)
      orderBy [asc (b ^. BookmarkTime)]
      pure
        ( b,
          subSelect $ from (table @BookmarkTag) >>= \t -> do
            where_ (t ^. BookmarkTagBookmarkId ==. b ^. BookmarkId)
            groupBy (t ^. BookmarkTagBookmarkId)
            orderBy [asc (t ^. BookmarkTagSeq)]
            pure $ sqliteGroupConcat (t ^. BookmarkTagTag) (val " ")
        )

parseSearchQuery ::
  (Text -> SqlExpr (Value Bool)) ->
  Text ->
  Maybe (SqlQuery ())
parseSearchQuery toExpr =
  fmap where_ . either (const Nothing) Just . P.parseOnly andE
  where
    andE = foldl1 (&&.) <$> P.many1 (P.skipSpace *> orE <|> tokenTermE)
    orE = foldl1 (||.) <$> tokenTermE `P.sepBy1` P.char '|'
    tokenTermE = negE termE <|> termE
      where
        negE p = not_ <$> (P.char '-' *> p)
        termE = toExpr <$> (fieldTerm <|> quotedTerm <|> simpleTerm)
        fieldTerm = concat <$> sequence [simpleTerm, P.string ":", quotedTerm <|> simpleTerm]
        quotedTerm = PC.between (P.char '"') (P.char '"') (P.takeWhile1 (/= '"'))
        simpleTerm = P.takeWhile1 (\c -> not (isSpace c) && c /= ':' && c /= '|')

parseTimeText :: (TI.ParseTime t, MonadFail m, Alternative m) => Text -> m t
parseTimeText t =
  asum
    $ flip (parseTimeM True defaultTimeLocale) (unpack t)
    <$> [ "%-m/%-d/%Y",
          "%-m/%-d/%Y%z",
          "%-m/%-d/%Y%Z", -- 12/31/2018
          "%Y-%-m-%-d",
          "%Y-%-m-%-d%z",
          "%Y-%-m-%-d%Z", -- 2018-12-31
          "%Y-%-m-%-dT%T",
          "%Y-%-m-%-dT%T%z",
          "%Y-%-m-%-dT%T%Z", -- 2018-12-31T06:40:53
          "%s" -- 1535932800
        ]

withTags :: Key Bookmark -> DB [Entity BookmarkTag]
withTags key = selectList [BookmarkTagBookmarkId CP.==. key] [Asc BookmarkTagSeq]

-- Note List Query

getNote :: Key User -> NtSlug -> DB (Maybe (Entity Note))
getNote userKey slug =
  selectFirst [NoteUserId CP.==. userKey, NoteSlug CP.==. slug] []

getNoteList :: Key User -> Maybe Text -> SharedP -> Limit -> Page -> DB (Int, [Entity Note])
getNoteList key mquery sharedp limit' page =
  (,) -- total count
    <$> fmap
      (sum . fmap unValue)
      ( select $ do
          b <- from (table @Note)
          _whereClause b
          pure countRows
      )
    <*> ( select $ do
            b <- from (table @Note)
            _whereClause b
            orderBy [desc (b ^. NoteCreated)]
            limit limit'
            offset ((page - 1) * limit')
            pure b
        )
  where
    _whereClause b = do
      where_ (b ^. NoteUserId ==. val key)
      -- search
      sequenceA_ (parseSearchQuery (toLikeExpr b) =<< mquery)
      case sharedp of
        SharedAll -> pure ()
        SharedPublic -> where_ (b ^. NoteShared ==. val True)
        SharedPrivate -> where_ (b ^. NoteShared ==. val False)

    toLikeExpr :: SqlExpr (Entity Note) -> Text -> SqlExpr (Value Bool)
    toLikeExpr b term = fromRight p_allFields (P.parseOnly p_onefield term)
      where
        wild s = (%) ++. val s ++. (%)
        toLikeN field s = b ^. field `like` wild s
        p_allFields = toLikeN NoteTitle term ||. toLikeN NoteText term
        p_onefield = p_title <|> p_text <|> p_after <|> p_before
          where
            p_title = "title:" *> fmap (toLikeN NoteTitle) P.takeText
            p_text = "description:" *> fmap (toLikeN NoteText) P.takeText
            p_after = "after:" *> fmap ((b ^. NoteCreated >=.) . val) (parseTimeText =<< P.takeText)
            p_before = "before:" *> fmap ((b ^. NoteCreated <=.) . val) (parseTimeText =<< P.takeText)

mkBookmarkTags :: Key User -> Key Bookmark -> [Tag] -> [BookmarkTag]
mkBookmarkTags userId bookmarkId tags =
  (\(i, tag) -> BookmarkTag userId tag bookmarkId i) <$> zip [1 ..] tags

-- * Bookmark Tag Cloud

data TagCloudMode
  = TagCloudModeTop Bool Int -- { mode: "top", value: 200 }
  | TagCloudModeLowerBound Bool Int -- { mode: "lowerBound", value: 20 }
  | TagCloudModeRelated Bool [Tag]
  | TagCloudModeNone
  deriving (Show, Eq, Read, Generic)

isExpanded :: TagCloudMode -> Bool
isExpanded (TagCloudModeTop e _) = e
isExpanded (TagCloudModeLowerBound e _) = e
isExpanded (TagCloudModeRelated e _) = e
isExpanded TagCloudModeNone = False

instance FromJSON TagCloudMode where
  parseJSON (Object o) =
    case KM.lookup "mode" o of
      Just (String "top") -> TagCloudModeTop <$> o .: "expanded" <*> o .: "value"
      Just (String "lowerBound") -> TagCloudModeLowerBound <$> o .: "expanded" <*> o .: "value"
      Just (String "related") -> TagCloudModeRelated <$> o .: "expanded" <*> fmap words (o .: "value")
      Just (String "none") -> pure TagCloudModeNone
      _ -> A.parseFail "bad parse"
  parseJSON _ = A.parseFail "bad parse"

instance ToJSON TagCloudMode where
  toJSON (TagCloudModeTop e i) =
    object
      [ "mode" .= String "top",
        "value" .= toJSON i,
        "expanded" .= Bool e
      ]
  toJSON (TagCloudModeLowerBound e i) =
    object
      [ "mode" .= String "lowerBound",
        "value" .= toJSON i,
        "expanded" .= Bool e
      ]
  toJSON (TagCloudModeRelated e tags) =
    object
      [ "mode" .= String "related",
        "value" .= String (unwords tags),
        "expanded" .= Bool e
      ]
  toJSON TagCloudModeNone =
    object
      [ "mode" .= String "none",
        "value" .= Null,
        "expanded" .= Bool False
      ]

type Tag = Text

tagCountTop :: Key User -> Int -> DB [(Text, Int)]
tagCountTop user top =
  sortOn (toLower . fst)
    . fmap (bimap unValue unValue)
    <$> ( select $ do
            t <- from (table @BookmarkTag)
            where_ (t ^. BookmarkTagUserId ==. val user)
            groupBy (lower_ $ t ^. BookmarkTagTag)
            let countRows' = countRows
            orderBy [desc countRows']
            limit (fromIntegral top)
            pure (t ^. BookmarkTagTag, countRows')
        )

tagCountLowerBound :: Key User -> Int -> DB [(Text, Int)]
tagCountLowerBound user lowerBound =
  fmap (bimap unValue unValue)
    <$> ( select $ do
            t <- from (table @BookmarkTag)
            where_ (t ^. BookmarkTagUserId ==. val user)
            groupBy (lower_ $ t ^. BookmarkTagTag)
            let countRows' = countRows
            orderBy [asc (t ^. BookmarkTagTag)]
            having (countRows' >=. val lowerBound)
            pure (t ^. BookmarkTagTag, countRows')
        )

tagCountRelated :: Key User -> [Tag] -> DB [(Text, Int)]
tagCountRelated user tags =
  fmap (bimap unValue unValue)
    <$> ( select $ do
            t <- from (table @BookmarkTag)
            where_
              $ foldl
                ( \expr tag ->
                    expr
                      &&. exists
                        ( do
                            u <- from (table @BookmarkTag)
                            where_
                              ( u ^. BookmarkTagBookmarkId ==. t ^. BookmarkTagBookmarkId
                                  &&. (u ^. BookmarkTagTag `like` val tag)
                              )
                        )
                )
                (t ^. BookmarkTagUserId ==. val user)
                tags
            groupBy (lower_ $ t ^. BookmarkTagTag)
            let countRows' = countRows
            orderBy [asc $ lower_ $ (t ^. BookmarkTagTag)]
            pure (t ^. BookmarkTagTag, countRows')
        )

fetchBookmarkByUrl :: Key User -> Maybe Text -> DB (Maybe (Entity Bookmark, [Entity BookmarkTag]))
fetchBookmarkByUrl userId murl = runMaybeT do
  bmark <- MaybeT . getBy . UniqueUserHref userId =<< MaybeT (pure murl)
  btags <- lift $ withTags (entityKey bmark)
  pure (bmark, btags)

data UpsertResult a = Created a | Updated a | Failed String
  deriving (Show, Eq, Functor)

maybeUpsertResult :: UpsertResult a -> Maybe a
maybeUpsertResult (Created a) = Just a
maybeUpsertResult (Updated a) = Just a
maybeUpsertResult _ = Nothing

upsertBookmark :: Key User -> Maybe (Key Bookmark) -> Bookmark -> [Text] -> DB (UpsertResult (Key Bookmark))
upsertBookmark userId mbid bm tags = do
  res <- case mbid of
    Just bid ->
      get bid >>= \case
        Just prev_bm
          | userId == bookmarkUserId prev_bm ->
              replaceBookmark bid prev_bm
        Just _ -> pure (Failed "unauthorized")
        _ -> pure (Failed "not found")
    Nothing ->
      getBy (UniqueUserHref (bookmarkUserId bm) (bookmarkHref bm)) >>= \case
        Just (Entity bid prev_bm) -> replaceBookmark bid prev_bm
        _ -> Created <$> insert bm
  forM_ (maybeUpsertResult res) (insertTags (bookmarkUserId bm))
  pure res
  where
    prepareReplace prev_bm =
      if bookmarkHref bm /= bookmarkHref prev_bm
        then bm {bookmarkArchiveHref = Nothing}
        else bm {bookmarkArchiveHref = bookmarkArchiveHref prev_bm}
    replaceBookmark bid prev_bm = do
      replace bid (prepareReplace prev_bm)
      deleteTags bid
      pure (Updated bid)
    deleteTags bid =
      deleteWhere [BookmarkTagBookmarkId CP.==. bid]
    insertTags userId' bid' =
      for_ (zip [1 ..] tags)
        $ \(i, tag) -> void $ insert $ BookmarkTag userId' tag bid' i

updateBookmarkArchiveUrl :: Key User -> Key Bookmark -> Maybe Text -> DB ()
updateBookmarkArchiveUrl userId bid marchiveUrl =
  updateWhere
    [BookmarkUserId CP.==. userId, BookmarkId CP.==. bid]
    [BookmarkArchiveHref CP.=. marchiveUrl]

upsertNote :: Key User -> Maybe (Key Note) -> Note -> DB (UpsertResult (Key Note))
upsertNote userId mnid note =
  case mnid of
    Just nid -> do
      get nid >>= \case
        Just note' -> do
          when
            (userId /= noteUserId note')
            (throwString "unauthorized")
          replace nid note
          pure (Updated nid)
        _ -> throwString "not found"
    Nothing -> do
      Created <$> insert note
