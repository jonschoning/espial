{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Model where

import qualified ClassyPrelude.Yesod as CP
import qualified Control.Monad.Combinators as PC (between)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as A (parseFail)
import qualified Data.Attoparsec.Text as P
import qualified Data.Time as TI (ParseTime)
import qualified Data.Time.Clock.POSIX as TI (posixSecondsToUTCTime, POSIXTime)
import qualified Data.Time.ISO8601 as TI (parseISO8601, formatISO8601Millis)
import ClassyPrelude.Yesod hiding ((==.), (||.), on, Value, groupBy, exists, (>=.), (<=.))
import Control.Monad.Fail (MonadFail)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Writer (tell)
import Data.Char (isSpace)
import Data.Either (fromRight)
import Data.Foldable (foldl, foldl1, sequenceA_)
import Data.List.NonEmpty (NonEmpty(..))
import Database.Esqueleto.Experimental
import Database.Esqueleto.Internal.Internal (unsafeSqlFunction)
import Pretty ()
import System.Directory (listDirectory)
import Types

import qualified Data.Map.Strict as MS

import ModelCustom

share [mkPersist sqlSettings, mkMigrate "migrateSchema"] [persistLowerCase|
User json
  Id Int64
  name Text
  passwordHash BCrypt
  apiToken HashedApiKey Maybe
  privateDefault Bool
  archiveDefault Bool
  privacyLock Bool
  UniqueUserName name
  deriving Show Eq Typeable Ord

Bookmark json
  Id Int64
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
  Id Int64
  userId UserId OnDeleteCascade
  tag Text
  bookmarkId BookmarkId OnDeleteCascade
  seq Int
  UniqueUserTagBookmarkId userId tag bookmarkId
  UniqueUserBookmarkIdTagSeq userId bookmarkId tag seq
  deriving Show Eq Typeable Ord

Note json
  Id Int64
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

newtype UTCTimeStr =
  UTCTimeStr { unUTCTimeStr :: UTCTime }
  deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

instance PathPiece UTCTimeStr where
  toPathPiece (UTCTimeStr u) = pack (TI.formatISO8601Millis u)
  fromPathPiece s = UTCTimeStr <$> TI.parseISO8601 (unpack s)

newtype UserNameP =
  UserNameP { unUserNameP :: Text }
  deriving (Eq, Show, Read)

newtype TagsP =
  TagsP { unTagsP :: [Text] }
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

newtype UnreadOnly =
  UnreadOnly { unUnreadOnly :: Bool }

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
toMigration = lift . tell . fmap (False ,)

migrateIndexes :: Migration
migrateIndexes =
  toMigration
    [ "CREATE INDEX IF NOT EXISTS idx_bookmark_time ON bookmark (user_id, time DESC)"
    , "CREATE INDEX IF NOT EXISTS idx_bookmark_tag_bookmark_id ON bookmark_tag (bookmark_id, id, tag, seq)"
    , "CREATE INDEX IF NOT EXISTS idx_note_user_created ON note (user_id, created DESC)"
    ]

sqliteGroupConcat ::
     PersistField a
  => SqlExpr (Value a)
  -> SqlExpr (Value a)
  -> SqlExpr (Value Text)
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
bookmarksTagsQuery
  :: Key User
  -> SharedP
  -> FilterP
  -> [Tag]
  -> Maybe Text
  -> Limit
  -> Page
  -> DB (Int, [(Entity Bookmark, Maybe Text)])
bookmarksTagsQuery userId sharedp filterp tags mquery limit' page =
  (,) -- total count
  <$> fmap (sum . fmap unValue)
      (select $ from (table @Bookmark) >>= \b -> do
       _whereClause b
       pure countRows)
      -- paged data
  <*> (fmap . fmap . fmap) unValue
      (select $ from (table @Bookmark) >>= \b -> do
       _whereClause b
       orderBy [desc (b ^. BookmarkTime)]
       limit limit'
       offset ((page - 1) * limit')
       pure (b, subSelect $ from (table @BookmarkTag) >>= \t -> do
                where_ (t ^. BookmarkTagBookmarkId ==. b ^. BookmarkId)
                groupBy (t ^. BookmarkTagBookmarkId)
                orderBy [asc (t ^. BookmarkTagSeq)]
                pure $ sqliteGroupConcat (t ^. BookmarkTagTag) (val " ")))
  where
    _whereClause b = do
      where_ $
        foldl (\expr tag ->
                expr &&. exists (   -- each tag becomes an exists constraint
                          from (table @BookmarkTag) >>= \t ->
                          where_ (t ^. BookmarkTagBookmarkId ==. b ^. BookmarkId &&.
                                 (t ^. BookmarkTagTag `like` val tag))))
          (b ^. BookmarkUserId ==. val userId)
          tags
      case sharedp of
        SharedAll -> pure ()
        SharedPublic ->  where_ (b ^. BookmarkShared ==. val True)
        SharedPrivate -> where_ (b ^. BookmarkShared ==. val False)
      case filterp of
        FilterAll -> pure ()
        FilterUnread -> where_ (b ^. BookmarkToRead ==. val True)
        FilterStarred -> where_ (b ^. BookmarkSelected ==. val True)
        FilterSingle slug -> where_ (b ^. BookmarkSlug ==. val slug)
        FilterUntagged -> where_ $ notExists $ from (table @BookmarkTag) >>= \t -> where_ $
                                                    t ^. BookmarkTagBookmarkId ==. b ^. BookmarkId
      -- search
      sequenceA_ (parseSearchQuery (toLikeExpr b) =<< mquery)

    toLikeExpr :: SqlExpr (Entity Bookmark) -> Text -> SqlExpr (Value Bool)
    toLikeExpr b term = fromRight p_allFields (P.parseOnly p_onefield term)
      where
        wild s = (%) ++. val s ++. (%)
        toLikeB field s = b ^. field `like` wild s
        p_allFields =
          toLikeB BookmarkHref term ||.
          toLikeB BookmarkDescription term ||.
          toLikeB BookmarkExtended term ||.
          exists (from (table @BookmarkTag) >>= \t -> where_ $
               (t ^. BookmarkTagBookmarkId ==. b ^. BookmarkId) &&.
               (t ^. BookmarkTagTag `like` wild term))
        p_onefield = p_url <|> p_title <|> p_description <|> p_tags <|> p_after <|> p_before
          where
            p_url = "url:" *> fmap (toLikeB BookmarkHref) P.takeText
            p_title = "title:" *> fmap (toLikeB BookmarkDescription) P.takeText
            p_description = "description:" *> fmap (toLikeB BookmarkExtended) P.takeText
            p_tags = "tags:" *> fmap (\term' -> exists $ from (table @BookmarkTag) >>= \t -> where_ $
                                                         (t ^. BookmarkTagBookmarkId ==. b ^. BookmarkId) &&.
                                                         (t ^. BookmarkTagTag `like` wild term')) P.takeText
            p_after  = "after:"  *> fmap ((b ^. BookmarkTime >=.) . val) (parseTimeText =<< P.takeText)
            p_before = "before:" *> fmap ((b ^. BookmarkTime <=.) . val) (parseTimeText =<< P.takeText)


-- returns a list of pair of bookmark with tags merged into a string
allUserBookmarks :: Key User -> DB [(Entity Bookmark, Text)]
allUserBookmarks user =
  (fmap . fmap . fmap) (fromMaybe "" . unValue) $
  select $ do
    b <- from (table @Bookmark)
    where_ (b ^. BookmarkUserId ==. val user)
    orderBy [asc (b ^. BookmarkTime)]
    pure (b, subSelect $ from (table @BookmarkTag) >>= \t -> do
             where_ (t ^. BookmarkTagBookmarkId ==. b ^. BookmarkId)
             groupBy (t ^. BookmarkTagBookmarkId)
             orderBy [asc (t ^. BookmarkTagSeq)]
             pure $ sqliteGroupConcat (t ^. BookmarkTagTag) (val " "))

parseSearchQuery ::
  (Text -> SqlExpr (Value Bool))
  -> Text
  -> Maybe (SqlQuery ())
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
  asum $
  flip (parseTimeM True defaultTimeLocale) (unpack t) <$>
  [ "%-m/%-d/%Y"    , "%-m/%-d/%Y%z"    , "%-m/%-d/%Y%Z"     -- 12/31/2018
  , "%Y-%-m-%-d"    , "%Y-%-m-%-d%z"    , "%Y-%-m-%-d%Z"     -- 2018-12-31
  , "%Y-%-m-%-dT%T" , "%Y-%-m-%-dT%T%z" , "%Y-%-m-%-dT%T%Z"  -- 2018-12-31T06:40:53
  , "%s"                                                     -- 1535932800
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
  <$> fmap (sum . fmap unValue)
      (select $ do
      b <- from (table @Note)
      _whereClause b
      pure countRows)
  <*> (select $ do
       b <- from (table @Note)
       _whereClause b
       orderBy [desc (b ^. NoteCreated)]
       limit limit'
       offset ((page - 1) * limit')
       pure b)
  where
    _whereClause b = do
      where_ (b ^. NoteUserId ==. val key)
      -- search
      sequenceA_ (parseSearchQuery (toLikeExpr b) =<< mquery)
      case sharedp of
        SharedAll -> pure ()
        SharedPublic ->  where_ (b ^. NoteShared ==. val True)
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
            p_after  = "after:"  *> fmap ((b ^. NoteCreated >=.) . val) (parseTimeText =<< P.takeText)
            p_before = "before:" *> fmap ((b ^. NoteCreated <=.) . val) (parseTimeText =<< P.takeText)

-- Bookmark Files

mkBookmarkTags :: Key User -> Key Bookmark -> [Tag] -> [BookmarkTag]
mkBookmarkTags userId bookmarkId tags =
  (\(i, tag) -> BookmarkTag userId tag bookmarkId i) <$> zip [1 ..] tags


fileBookmarkToBookmark :: UserId -> FileBookmark -> IO Bookmark
fileBookmarkToBookmark user FileBookmark {..} = do
  slug <- mkBmSlug
  pure $
    Bookmark
    { bookmarkUserId = user
    , bookmarkSlug = slug
    , bookmarkHref = fileBookmarkHref
    , bookmarkDescription = fileBookmarkDescription
    , bookmarkExtended = fileBookmarkExtended
    , bookmarkTime = fileBookmarkTime
    , bookmarkShared = fileBookmarkShared
    , bookmarkToRead = fileBookmarkToRead
    , bookmarkSelected = Just True == fileBookmarkSelected
    , bookmarkArchiveHref = fileBookmarkArchiveHref
    }

bookmarkTofileBookmark :: Bookmark -> Text -> FileBookmark
bookmarkTofileBookmark Bookmark {..} tags =
    FileBookmark
    { fileBookmarkHref = bookmarkHref
    , fileBookmarkDescription = bookmarkDescription
    , fileBookmarkExtended = bookmarkExtended
    , fileBookmarkTime = bookmarkTime
    , fileBookmarkShared = bookmarkShared
    , fileBookmarkToRead = bookmarkToRead
    , fileBookmarkSelected = Just bookmarkSelected
    , fileBookmarkArchiveHref = bookmarkArchiveHref
    , fileBookmarkTags = tags
    }

data FFBookmarkNode = FFBookmarkNode
  { firefoxBookmarkChildren :: Maybe [FFBookmarkNode]
  , firefoxBookmarkDateAdded :: !TI.POSIXTime
  , firefoxBookmarkGuid :: !Text
  , firefoxBookmarkIconUri :: !(Maybe Text)
  , firefoxBookmarkId :: !Int
  , firefoxBookmarkIndex :: !Int
  , firefoxBookmarkLastModified :: !TI.POSIXTime
  , firefoxBookmarkRoot :: !(Maybe Text)
  , firefoxBookmarkTitle :: !Text
  , firefoxBookmarkType :: !Text
  , firefoxBookmarkTypeCode :: !Int
  , firefoxBookmarkUri :: !(Maybe Text)
  } deriving (Show, Eq, Typeable, Ord)

instance FromJSON FFBookmarkNode where
  parseJSON (Object o) =
    FFBookmarkNode <$>
    (o A..:? "children") <*>
    (o .: "dateAdded") <*>
    o .: "guid" <*>
    (o A..:? "iconUri") <*>
    o .: "id" <*>
    o .: "index" <*>
    (o .: "lastModified") <*>
    (o A..:? "root") <*>
    (o .: "title") <*>
    (o .: "type") <*>
    (o .: "typeCode") <*>
    (o A..:? "uri")
  parseJSON _ = A.parseFail "bad parse"

firefoxBookmarkNodeToBookmark :: UserId -> FFBookmarkNode -> IO [Bookmark]
firefoxBookmarkNodeToBookmark user FFBookmarkNode {..} =
  case firefoxBookmarkTypeCode of
    1 -> do
      slug <- mkBmSlug
      pure $
        [ Bookmark
          { bookmarkUserId = user
          , bookmarkSlug = slug
          , bookmarkHref = fromMaybe "" firefoxBookmarkUri
          , bookmarkDescription = firefoxBookmarkTitle
          , bookmarkExtended = ""
          , bookmarkTime = TI.posixSecondsToUTCTime (firefoxBookmarkDateAdded / 1000000)
          , bookmarkShared = True
          , bookmarkToRead = False
          , bookmarkSelected = False
          , bookmarkArchiveHref = Nothing
          }
        ]
    2 ->
      join <$>
      mapM
        (firefoxBookmarkNodeToBookmark user)
        (fromMaybe [] firefoxBookmarkChildren)
    _ -> pure []


insertFileBookmarks :: Key User -> FilePath -> DB (Either String Int)
insertFileBookmarks userId bookmarkFile = do
  mfmarks <- liftIO $ readFileBookmarks bookmarkFile
  case mfmarks of
    Left e -> pure $ Left e
    Right fmarks -> do
      bmarks <- liftIO $ mapM (fileBookmarkToBookmark userId) fmarks
      mbids <- mapM insertUnique bmarks
      mapM_ (void . insertUnique) $
        concatMap (uncurry (mkBookmarkTags userId)) $
        catMaybes $
        zipWith
          (\mbid tags -> (, tags) <$> mbid)
          mbids
          (extractTags <$> fmarks)
      pure $ Right (length bmarks)

  where
    extractTags = words . fileBookmarkTags

insertFFBookmarks :: Key User -> FilePath -> DB (Either String Int)
insertFFBookmarks userId bookmarkFile = do
  mfmarks <- liftIO $ readFFBookmarks bookmarkFile
  case mfmarks of
    Left e -> pure $ Left e
    Right fmarks -> do
      bmarks <- liftIO $ firefoxBookmarkNodeToBookmark userId fmarks
      mapM_ (void . insertUnique) bmarks
      pure $ Right (length bmarks)


readFileBookmarks :: MonadIO m => FilePath -> m (Either String [FileBookmark])
readFileBookmarks fpath =
  A.eitherDecode' . fromStrict <$> readFile fpath

readFFBookmarks :: MonadIO m => FilePath -> m (Either String FFBookmarkNode)
readFFBookmarks fpath =
  A.eitherDecode' . fromStrict <$> readFile fpath

exportFileBookmarks :: Key User -> FilePath -> DB ()
exportFileBookmarks user fpath =
    liftIO . A.encodeFile fpath =<< getFileBookmarks user

getFileBookmarks :: Key User -> DB [FileBookmark]
getFileBookmarks user = do
  marks <- allUserBookmarks user
  pure $ fmap (\(bm, t) -> bookmarkTofileBookmark (entityVal bm) t) marks

data TagCloudMode
  = TagCloudModeTop Bool Int          -- { mode: "top", value: 200 }
  | TagCloudModeLowerBound Bool Int   -- { mode: "lowerBound", value: 20 }
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
    object [ "mode" .= String "top"
           , "value" .= toJSON i
           , "expanded" .= Bool e
           ]
  toJSON (TagCloudModeLowerBound e i) =
    object [ "mode" .= String "lowerBound"
           , "value" .= toJSON i
           , "expanded" .= Bool e
           ]
  toJSON (TagCloudModeRelated e tags) =
    object [ "mode" .= String "related"
           , "value" .= String (unwords tags)
           , "expanded" .= Bool e
           ]
  toJSON TagCloudModeNone =
    object [ "mode" .= String "none"
           , "value" .= Null
           , "expanded" .= Bool False
           ]


type Tag = Text

tagCountTop :: Key User -> Int -> DB [(Text, Int)]
tagCountTop user top =
    sortOn (toLower . fst) .
    fmap (bimap unValue unValue) <$>
    ( select $ do
      t <- from (table @BookmarkTag)
      where_ (t ^. BookmarkTagUserId ==. val user)
      groupBy (lower_ $ t ^. BookmarkTagTag)
      let countRows' = countRows
      orderBy [desc countRows']
      limit ((fromIntegral . toInteger) top)
      pure (t ^. BookmarkTagTag, countRows')
    )

tagCountLowerBound :: Key User -> Int -> DB [(Text, Int)]
tagCountLowerBound user lowerBound =
    fmap (bimap unValue unValue) <$>
    ( select $ do
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
    fmap (bimap unValue unValue) <$>
    ( select $ do
      t <- from (table @BookmarkTag)
      where_ $
        foldl (\expr tag ->
                expr &&. exists ( do
                          u <- from (table @BookmarkTag)
                          where_ (u ^. BookmarkTagBookmarkId ==. t ^. BookmarkTagBookmarkId &&.
                                 (u ^. BookmarkTagTag `like` val tag))))
          (t ^. BookmarkTagUserId ==. val user)
          tags
      groupBy (lower_ $ t ^. BookmarkTagTag)
      let countRows' = countRows
      orderBy [asc $ lower_ $ (t ^. BookmarkTagTag)]
      pure (t ^. BookmarkTagTag, countRows')
    )

-- Notes

fileNoteToNote :: UserId -> FileNote -> IO Note
fileNoteToNote user FileNote {..}  = do
  slug <- mkNtSlug
  pure $
    Note
    { noteUserId = user
    , noteSlug = slug
    , noteLength = fileNoteLength
    , noteTitle = fileNoteTitle
    , noteText = fileNoteText
    , noteIsMarkdown = False
    , noteShared = False
    , noteCreated = fileNoteCreatedAt
    , noteUpdated = fileNoteUpdatedAt
    }

insertDirFileNotes :: Key User -> FilePath -> DB (Either String Int)
insertDirFileNotes userId noteDirectory = do
  mfnotes <- liftIO $ readFileNotes noteDirectory
  case mfnotes of
      Left e -> pure $ Left e
      Right fnotes -> do
        notes <- liftIO $ mapM (fileNoteToNote userId) fnotes
        void $ mapM insertUnique notes
        pure $ Right (length notes)
  where
    readFileNotes :: MonadIO m => FilePath -> m (Either String [FileNote])
    readFileNotes fdir = do
      files <- liftIO (listDirectory fdir)
      noteBSS <- mapM (readFile . (fdir </>)) files
      pure (mapM (A.eitherDecode' . fromStrict) noteBSS)

-- AccountSettingsForm
data AccountSettingsForm = AccountSettingsForm
  { _privateDefault :: Bool
  , _archiveDefault :: Bool
  , _privacyLock :: Bool
  } deriving (Show, Eq, Read, Generic)

instance FromJSON AccountSettingsForm where parseJSON = A.genericParseJSON gDefaultFormOptions
instance ToJSON AccountSettingsForm where toJSON = A.genericToJSON gDefaultFormOptions

toAccountSettingsForm :: User -> AccountSettingsForm
toAccountSettingsForm User {..} =
  AccountSettingsForm
  { _privateDefault = userPrivateDefault
  , _archiveDefault = userArchiveDefault
  , _privacyLock = userPrivacyLock
  }

updateUserFromAccountSettingsForm :: Key User -> AccountSettingsForm -> DB ()
updateUserFromAccountSettingsForm userId AccountSettingsForm {..} =
  CP.update userId
  [ UserPrivateDefault CP.=. _privateDefault
  , UserArchiveDefault CP.=. _archiveDefault
  , UserPrivacyLock CP.=. _privacyLock
  ]

-- BookmarkForm

data BookmarkForm = BookmarkForm
  { _url :: Text
  , _title :: Maybe Text
  , _description :: Maybe Textarea
  , _tags :: Maybe Text
  , _private :: Maybe Bool
  , _toread :: Maybe Bool
  , _bid :: Maybe Int64
  , _slug :: Maybe BmSlug
  , _selected :: Maybe Bool
  , _time :: Maybe UTCTimeStr
  , _archiveUrl :: Maybe Text
  } deriving (Show, Eq, Read, Generic)

instance FromJSON BookmarkForm where parseJSON = A.genericParseJSON gDefaultFormOptions
instance ToJSON BookmarkForm where toJSON = A.genericToJSON gDefaultFormOptions

gDefaultFormOptions :: A.Options
gDefaultFormOptions = A.defaultOptions { A.fieldLabelModifier = drop 1 }

toBookmarkFormList :: [(Entity Bookmark, Maybe Text)] -> [BookmarkForm]
toBookmarkFormList = fmap _toBookmarkForm'

_toBookmarkForm :: (Entity Bookmark, [Entity BookmarkTag]) -> BookmarkForm
_toBookmarkForm (bm, tags) =
  _toBookmarkForm' (bm, Just $ unwords $ fmap (bookmarkTagTag . entityVal) tags)

_toBookmarkForm' :: (Entity Bookmark, Maybe Text) -> BookmarkForm
_toBookmarkForm' (Entity bid Bookmark {..}, tags) =
  BookmarkForm
  { _url = bookmarkHref
  , _title = Just bookmarkDescription
  , _description = Just $ Textarea $ bookmarkExtended
  , _tags = Just $ fromMaybe "" tags
  , _private = Just $ not bookmarkShared
  , _toread = Just bookmarkToRead
  , _bid = Just $ unBookmarkKey $ bid
  , _slug = Just bookmarkSlug
  , _selected = Just bookmarkSelected
  , _time = Just $ UTCTimeStr $ bookmarkTime
  , _archiveUrl = bookmarkArchiveHref
  }


_toBookmark :: UserId -> BookmarkForm -> IO Bookmark
_toBookmark userId BookmarkForm {..} = do
  time <- liftIO getCurrentTime
  slug <- maybe mkBmSlug pure _slug
  pure $
    Bookmark
    { bookmarkUserId = userId
    , bookmarkSlug = slug
    , bookmarkHref = _url
    , bookmarkDescription = fromMaybe "" _title
    , bookmarkExtended = maybe "" unTextarea _description
    , bookmarkTime = maybe time unUTCTimeStr _time
    , bookmarkShared = maybe True not _private
    , bookmarkToRead = Just True == _toread
    , bookmarkSelected = Just True == _selected
    , bookmarkArchiveHref = _archiveUrl
    }

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
        Just prev_bm | userId == bookmarkUserId prev_bm ->
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
        then bm { bookmarkArchiveHref = Nothing }
        else bm { bookmarkArchiveHref = bookmarkArchiveHref prev_bm }
    replaceBookmark bid prev_bm = do
      replace bid (prepareReplace prev_bm)
      deleteTags bid
      pure (Updated bid)
    deleteTags bid =
      deleteWhere [BookmarkTagBookmarkId CP.==. bid]
    insertTags userId' bid' =
      for_ (zip [1 ..] tags) $
      \(i, tag) -> void $ insert $ BookmarkTag userId' tag bid' i

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
          when (userId /= noteUserId note')
            (throwString "unauthorized")
          replace nid note
          pure (Updated nid)
        _ -> throwString "not found"
    Nothing -> do
      Created <$> insert note

-- * FileBookmarks

data FileBookmark = FileBookmark
  { fileBookmarkHref :: !Text
  , fileBookmarkDescription :: !Text
  , fileBookmarkExtended :: !Text
  , fileBookmarkTime :: !UTCTime
  , fileBookmarkShared :: !Bool
  , fileBookmarkToRead :: !Bool
  , fileBookmarkSelected :: !(Maybe Bool)
  , fileBookmarkArchiveHref :: !(Maybe Text)
  , fileBookmarkTags :: !Text
  } deriving (Show, Eq, Typeable, Ord)

instance FromJSON FileBookmark where
  parseJSON (Object o) =
    FileBookmark <$> o .: "href" <*> o .: "description" <*> o .: "extended" <*>
    o .: "time" <*>
    (boolFromYesNo <$> o .: "shared") <*>
    (boolFromYesNo <$> o .: "toread") <*>
    (o A..:? "selected") <*>
    (o A..:? "archive_url") <*>
    (o .: "tags")
  parseJSON _ = A.parseFail "bad parse"

instance ToJSON FileBookmark where
  toJSON FileBookmark {..} =
    object
      [ "href" .= toJSON fileBookmarkHref
      , "description" .= toJSON fileBookmarkDescription
      , "extended" .= toJSON fileBookmarkExtended
      , "time" .= toJSON fileBookmarkTime
      , "shared" .= toJSON (boolToYesNo fileBookmarkShared)
      , "toread" .= toJSON (boolToYesNo fileBookmarkToRead)
      , "selected" .= toJSON fileBookmarkSelected
      , "archive_url" .= toJSON fileBookmarkArchiveHref
      , "tags" .= toJSON fileBookmarkTags
      ]

boolFromYesNo :: Text -> Bool
boolFromYesNo "yes" = True
boolFromYesNo _ = False

boolToYesNo :: Bool -> Text
boolToYesNo True = "yes"
boolToYesNo _ = "no"

-- * FileNotes

data FileNote = FileNote
  { fileNoteId :: !Text
  , fileNoteTitle :: !Text
  , fileNoteText :: !Text
  , fileNoteLength :: !Int
  , fileNoteCreatedAt :: !UTCTime
  , fileNoteUpdatedAt :: !UTCTime
  } deriving (Show, Eq, Typeable, Ord)

instance FromJSON FileNote where
  parseJSON (Object o) =
    FileNote <$> o .: "id" <*> o .: "title" <*> o .: "text" <*>
    o .: "length" <*>
    (readFileNoteTime =<< o .: "created_at") <*>
    (readFileNoteTime =<< o .: "updated_at")
  parseJSON _ = A.parseFail "bad parse"

instance ToJSON FileNote where
  toJSON FileNote {..} =
    object
      [ "id" .= toJSON fileNoteId
      , "title" .= toJSON fileNoteTitle
      , "text" .= toJSON fileNoteText
      , "length" .= toJSON fileNoteLength
      , "created_at" .= toJSON (showFileNoteTime fileNoteCreatedAt)
      , "updated_at" .= toJSON (showFileNoteTime fileNoteUpdatedAt)
      ]

readFileNoteTime
  :: MonadFail m
  => String -> m UTCTime
readFileNoteTime = parseTimeM True defaultTimeLocale "%F %T"

showFileNoteTime :: UTCTime -> String
showFileNoteTime = formatTime defaultTimeLocale "%F %T"
