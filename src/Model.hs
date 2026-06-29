{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Model where

import ClassyPrelude.Yesod hiding (Value, delete, exists, groupBy, on, update, (<=.), (=.), (==.), (>=.), (||.))
import ClassyPrelude.Yesod qualified as CP
import Control.Monad (fail)
import Control.Monad.Combinators qualified as PC (between)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as A (parseFail)
import Data.Attoparsec.Text qualified as P
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.Char (isSpace)
import Data.Either (fromRight)
import Data.Foldable (foldl, foldl1, sequenceA_)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time qualified as TI (ParseTime)
import Data.Time.ISO8601 qualified as TISO (formatISO8601Millis, parseISO8601)
import Database.Esqueleto.Experimental hiding ((<&>))
import Database.Esqueleto.Internal.Internal (unsafeSqlFunction)
import Model.Custom
import Text.Blaze (ToMarkup, toMarkup)
import Types
import Util

-- * Database models

share
  [mkPersist sqlSettings, mkMigrate "migrateSchema"]
  [persistLowerCase|

AppMigration
    dbVersion Int
    migrationDescription Text
    appVersionSpec Text
    appliedAt UTCTime
    UniqueAppMigrationDbVersion dbVersion
    deriving Show Eq Typeable Ord

AppVersion
    appVersionSpec Text
    appVersion Text
    appGitSha Text
    created UTCTime
    deriving Show Eq Typeable Ord

User json
  name Text
  passwordHash BCrypt
  apiToken HashedApiKey Maybe
  privateDefault Bool
  archiveDefault Bool
  suggestTags Bool default=True
  privacyLock Bool
  language I18nLang Maybe
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

-- * Custom types

newtype UserAgent
  = UserAgent {unUserAgent :: Text}
  deriving (Eq, Show, Read)

newtype Url
  = Url {unUrl :: Text}
  deriving (Eq, Show, Read)

type Limit = Int64

type Page = Int64

-- Path/Routing Types

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

instance ToJSON SharedP where
  toJSON SharedAll = "all"
  toJSON SharedPublic = "public"
  toJSON SharedPrivate = "private"

instance FromJSON SharedP where
  parseJSON = A.withText "SharedP" \case
    "all" -> pure SharedAll
    "public" -> pure SharedPublic
    "private" -> pure SharedPrivate
    _ -> empty

data FilterP
  = FilterAll
  | FilterUnread
  | FilterUntagged
  | FilterStarred
  | FilterSingle BmSlug
  deriving (Eq, Show, Read, Generic)

instance FromJSON FilterP where parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON FilterP where toJSON = A.genericToJSON A.defaultOptions

-- * Paging

data PagingCursor a
  = PagingCursorBefore a
  | PagingCursorAfter a
  deriving (Eq, Show, Read)

type BookmarkPagingCursor = PagingCursor BookmarkId

type BookmarkPagingCursorTime = PagingCursor UTCTime

type NotePagingCursor = PagingCursor NoteId

type NotePagingCursorTime = PagingCursor UTCTime

-- * I18n

-- | Translation map: language -> namespace -> key -> translation
type I18nMap = HashMap I18nLang (HashMap I18nNs (HashMap I18nKey Text))

-- | Translation namespace
newtype I18nNs = I18nNs {unI18nNs :: Text} deriving (Eq, Ord, Show, Hashable, FromJSON, ToJSON)

-- | Translation key
newtype I18nKey = I18nKey {unI18nKey :: Text} deriving (Eq, Ord, Show, Hashable)

-- | Supported languages for translation
data I18nLang
  = -- | English
    I18nLangEn
  | -- | German
    I18nLangDe
  | -- | Spanish
    I18nLangEs
  | -- | French
    I18nLangFr
  | -- | Italian
    I18nLangIt
  | -- | Japanese
    I18nLangJa
  | -- | Korean
    I18nLangKo
  | -- | Polish
    I18nLangPl
  | -- | Portuguese (Brazil)
    I18nLangPtBr
  | -- | Russian
    I18nLangRu
  | -- | Turkish
    I18nLangTr
  | -- | Ukrainian
    I18nLangUk
  | -- | Chinese (Simplified)
    I18nLangZhHans
  | -- | Chinese (Traditional)
    I18nLangZhHant
  deriving (Eq, Ord, Enum, Bounded, Generic)

instance Hashable I18nLang where hashWithSalt s = hashWithSalt s . fromEnum

instance ToMarkup I18nLang where toMarkup = toMarkup . fromI18nLang'

instance PersistField I18nLang where
  toPersistValue = PersistText . fromI18nLang'
  fromPersistValue (PersistText t) =
    maybe
      (Right I18nLangEn) -- in case of version downgrade, default to English if the language is not recognized
      Right
      (toI18nLang t)
  fromPersistValue _ = Left "Invalid PersistValue for I18nLang"

instance PersistFieldSql I18nLang where sqlType _ = SqlString

fromI18nLang :: (IsString a) => I18nLang -> a
fromI18nLang = \case
  I18nLangEn -> "en"
  I18nLangDe -> "de"
  I18nLangEs -> "es"
  I18nLangFr -> "fr"
  I18nLangIt -> "it"
  I18nLangJa -> "ja"
  I18nLangKo -> "ko"
  I18nLangPl -> "pl"
  I18nLangPtBr -> "pt-BR"
  I18nLangRu -> "ru"
  I18nLangTr -> "tr"
  I18nLangUk -> "uk"
  I18nLangZhHans -> "zh-Hans"
  I18nLangZhHant -> "zh-Hant"

fromI18nLang' :: I18nLang -> Text
fromI18nLang' = fromI18nLang

toI18nLang :: (Ord s, IsString s) => s -> Maybe I18nLang
toI18nLang s = inverseMap fromI18nLang s

instance Show I18nLang where show = fromI18nLang

instance ToJSON I18nLang where toJSON = A.String . fromI18nLang

instance FromJSON I18nLang where
  parseJSON = A.withText "I18nLang" $ \s ->
    maybe
      (fail $ "Invalid language: '" <> unpack s <> "'. Supported languages: " <> supportedLangs)
      pure
      (toI18nLang s)

supportedLangs :: String
supportedLangs = intercalate ", " (fmap fromI18nLang [minBound .. maxBound])

-- * Model functions

-- | Maximum number of parameters for SQLite queries
sqliteMaxParameters :: Int
sqliteMaxParameters = 32000

-- | SQLite GROUP_CONCAT function for concatenating values with a separator
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
  Maybe BookmarkPagingCursorTime ->
  Limit ->
  Page ->
  DB (Int, [(Entity Bookmark, Maybe Text)], Bool, Bool)
bookmarksTagsQuery userId isowner sharedp filterp tags mquery mcursor limit' page = do
  total <-
    fmap
      (sum . fmap unValue)
      ( select $ from (table @Bookmark) >>= \b -> do
          bookmarkWhereClause userId sharedp filterp tags mquery b
          pure countRows
      )
  rows <-
    (fmap . fmap . fmap)
      unValue
      ( select $ from (table @Bookmark) >>= \b -> do
          bookmarkWhereClause userId sharedp filterp tags mquery b
          for_ mcursor $ \case
            PagingCursorBefore before -> where_ (isBeforeCursor b before)
            PagingCursorAfter after -> where_ (isAfterCursor b after)
          orderBy (bookmarkOrder b)
          limit (limit' + 1) -- fetch one extra row to determine if there are more rows in the given direction
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
  let hasMoreInQueryDirection = fromIntegral (length rows) > limit'
      pageRows = take (fromIntegral limit') rows
      hasEarlier =
        case mcursor of
          Just (PagingCursorAfter _) -> not (null pageRows)
          _ -> hasMoreInQueryDirection
      hasLater =
        case mcursor of
          Just (PagingCursorAfter _) -> hasMoreInQueryDirection
          Just (PagingCursorBefore _) -> not (null pageRows)
          Nothing -> False
  pure (total, finalizeRows pageRows, hasEarlier, hasLater)
  where
    bookmarkOrder b = case mcursor of
      Just (PagingCursorAfter _) -> [asc (b ^. BookmarkTime), asc (b ^. BookmarkId)]
      _ -> [desc (b ^. BookmarkTime), desc (b ^. BookmarkId)]
    finalizeRows = case mcursor of
      Just (PagingCursorAfter _) -> reverse -- because of asc order fetch for after cursor, reverse to get newest first order
      _ -> id
    isBeforeCursor b before =
      ((b ^. BookmarkTime) Database.Esqueleto.Experimental.<. val before)
    isAfterCursor b after =
      ((b ^. BookmarkTime) Database.Esqueleto.Experimental.>. val after)

bookmarkWhereClause ::
  Key User ->
  SharedP ->
  FilterP ->
  [Tag] ->
  Maybe Text ->
  SqlExpr (Entity Bookmark) ->
  SqlQuery ()
bookmarkWhereClause userId sharedp filterp tags mquery b = do
  where_
    $ foldl
      ( \expr tag ->
          expr
            &&. exists
              ( from (table @BookmarkTag) >>= \t ->
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
        where_ $ t ^. BookmarkTagBookmarkId ==. b ^. BookmarkId
  sequenceA_ (parseSearchQuery (bookmarkLikeExpr b) =<< mquery)

bookmarkLikeExpr :: SqlExpr (Entity Bookmark) -> Text -> SqlExpr (Value Bool)
bookmarkLikeExpr b term = fromRight p_allFields (P.parseOnly p_onefield term)
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
        p_url = ("url:" <|> "u:") *> fmap (toLikeB BookmarkHref) P.takeText
        p_title = ("title:" <|> "ti:") *> fmap (toLikeB BookmarkDescription) P.takeText
        p_description = ("description:" <|> "d:") *> fmap (toLikeB BookmarkExtended) P.takeText
        p_tags =
          ("tags:" <|> "t:")
            *> fmap
              ( \term' ->
                  exists $ from (table @BookmarkTag) >>= \t ->
                    where_
                      $ (t ^. BookmarkTagBookmarkId ==. b ^. BookmarkId)
                      &&. (t ^. BookmarkTagTag `like` wild term')
              )
              P.takeText
        p_after = ("after:" <|> "a:") *> fmap ((b ^. BookmarkTime >=.) . val) (parseTimeText =<< P.takeText)
        p_before = ("before:" <|> "b:") *> fmap ((b ^. BookmarkTime <=.) . val) (parseTimeText =<< P.takeText)

-- * BulkEdit types

data BulkEditError
  = BulkEditErrorPageMismatch
  deriving (Show, Eq)

data BulkSelection
  = BulkSelectionPage
      -- | selected bookmark IDs on the current page
      [Int64]
  | BulkSelectionAll
      -- | filter
      FilterP
      -- | shared
      SharedP
      -- | tags
      [Tag]
      -- | query
      (Maybe Text)
  deriving (Show, Eq)

data BulkAction = BulkActionRead | BulkActionUnread | BulkActionStar | BulkActionUnstar | BulkActionDelete | BulkActionPrivate | BulkActionPublic
  deriving (Show, Eq, Generic)

data BulkEditForm = BulkEditForm
  { _beSelection :: BulkSelection,
    _beAction :: Maybe BulkAction,
    _beAddTags :: Text,
    _beRemoveTags :: Text,
    _beSelectionCount :: Int
  }
  deriving (Show, Eq)

instance FromJSON BulkAction where
  parseJSON = A.withText "BulkAction" \case
    "read" -> pure BulkActionRead
    "unread" -> pure BulkActionUnread
    "star" -> pure BulkActionStar
    "unstar" -> pure BulkActionUnstar
    "delete" -> pure BulkActionDelete
    "private" -> pure BulkActionPrivate
    "public" -> pure BulkActionPublic
    _ -> empty

instance FromJSON BulkEditForm where
  parseJSON = A.withObject "BulkEditForm" \o -> do
    selTag <- o .: "selection"
    selection <- case (selTag :: Text) of
      "page" -> BulkSelectionPage <$> o .: "bids"
      "all" ->
        BulkSelectionAll
          <$> o
          .: "filter"
          <*> o
          .: "sharedp"
          <*> o
          .: "tags"
          <*> o
          A..:? "query"
      _ -> fail $ "Unknown selection: " <> unpack selTag
    BulkEditForm selection
      <$> o
      A..:? "action"
      <*> o
      .: "addTags"
      <*> o
      .: "removeTags"
      <*> o
      .: "selectionCount"

bookmarksBulkEdit :: Key User -> BulkEditForm -> DB (Either BulkEditError Int)
bookmarksBulkEdit userId BulkEditForm {..} = do
  eValid <- validateCount
  case eValid of
    Left err -> return (Left err)
    Right n -> do
      let removeTags = normalizeTags _beRemoveTags
          addTags' = normalizeTags _beAddTags
      mTagKbids <-
        if null removeTags && null addTags'
          then pure Nothing
          else Just <$> resolveKbids
      forM_ _beAction applyAction
      forM_ mTagKbids $ \kbids -> do
        unless (null removeTags) $ applyRemoveTags kbids removeTags
        unless (null addTags') $ applyAddTags kbids addTags'
      return (Right n)
  where
    toKbids = map (toSqlKey @Bookmark)

    validateCount = case _beSelection of
      BulkSelectionPage bids ->
        if length bids /= _beSelectionCount
          then return $ Left BulkEditErrorPageMismatch
          else return (Right (length bids))
      BulkSelectionAll fp sp ts q -> do
        result <-
          select $ from (table @Bookmark) >>= \b -> do
            bookmarkWhereClause userId sp fp ts q b
            pure countRows
        return $ Right $ maybe 0 unValue (listToMaybe result)

    resolveKbids = case _beSelection of
      BulkSelectionPage bids -> return (toKbids bids)
      BulkSelectionAll fp sp ts q ->
        fmap (map unValue)
          $ select
          $ from (table @Bookmark)
          >>= \b -> do
            bookmarkWhereClause userId sp fp ts q b
            pure (b ^. BookmarkId)

    applyAction = \case
      BulkActionRead -> case _beSelection of
        BulkSelectionPage bids ->
          CP.updateWhere [BookmarkId CP.<-. toKbids bids, BookmarkUserId CP.==. userId] [BookmarkToRead CP.=. False]
        BulkSelectionAll fp sp ts q ->
          update $ \b -> do
            set b [BookmarkToRead =. val False]
            bookmarkWhereClause userId sp fp ts q b
      BulkActionUnread -> case _beSelection of
        BulkSelectionPage bids ->
          CP.updateWhere [BookmarkId CP.<-. toKbids bids, BookmarkUserId CP.==. userId] [BookmarkToRead CP.=. True]
        BulkSelectionAll fp sp ts q ->
          update $ \b -> do
            set b [BookmarkToRead =. val True]
            bookmarkWhereClause userId sp fp ts q b
      BulkActionStar -> case _beSelection of
        BulkSelectionPage bids ->
          CP.updateWhere [BookmarkId CP.<-. toKbids bids, BookmarkUserId CP.==. userId] [BookmarkSelected CP.=. True]
        BulkSelectionAll fp sp ts q ->
          update $ \b -> do
            set b [BookmarkSelected =. val True]
            bookmarkWhereClause userId sp fp ts q b
      BulkActionUnstar -> case _beSelection of
        BulkSelectionPage bids ->
          CP.updateWhere [BookmarkId CP.<-. toKbids bids, BookmarkUserId CP.==. userId] [BookmarkSelected CP.=. False]
        BulkSelectionAll fp sp ts q ->
          update $ \b -> do
            set b [BookmarkSelected =. val False]
            bookmarkWhereClause userId sp fp ts q b
      BulkActionDelete -> case _beSelection of
        BulkSelectionPage bids ->
          CP.deleteWhere [BookmarkId CP.<-. toKbids bids, BookmarkUserId CP.==. userId]
        BulkSelectionAll fp sp ts q ->
          delete $ from (table @Bookmark) >>= bookmarkWhereClause userId sp fp ts q
      BulkActionPrivate -> case _beSelection of
        BulkSelectionPage bids ->
          CP.updateWhere [BookmarkId CP.<-. toKbids bids, BookmarkUserId CP.==. userId] [BookmarkShared CP.=. False]
        BulkSelectionAll fp sp ts q ->
          update $ \b -> do
            set b [BookmarkShared =. val False]
            bookmarkWhereClause userId sp fp ts q b
      BulkActionPublic -> case _beSelection of
        BulkSelectionPage bids ->
          CP.updateWhere [BookmarkId CP.<-. toKbids bids, BookmarkUserId CP.==. userId] [BookmarkShared CP.=. True]
        BulkSelectionAll fp sp ts q ->
          update $ \b -> do
            set b [BookmarkShared =. val True]
            bookmarkWhereClause userId sp fp ts q b

    applyRemoveTags kbids removeTags =
      mapM_
        ( \batch ->
            delete
              $ from (table @BookmarkTag)
              >>= \t -> do
                where_ (t ^. BookmarkTagBookmarkId `in_` valList batch)
                where_ (t ^. BookmarkTagUserId ==. val userId)
                where_
                  $ foldl1 (||.)
                  $ map (\tag -> t ^. BookmarkTagTag `like` val tag) removeTags
        )
        (batchOf sqliteMaxParameters kbids)

    applyAddTags kbids addTags' =
      mapM_ (`addTagsToBids` addTags') (batchOf sqliteMaxParameters kbids)

    addTagsToBids kbids addTags' = do
      existingTags <-
        selectList
          [BookmarkTagBookmarkId CP.<-. kbids, BookmarkTagUserId CP.==. userId]
          [Asc BookmarkTagSeq]
      let bidTagMap :: Map BookmarkId (Int, Set (CI Tag))
          bidTagMap =
            foldl
              ( \m (Entity _ bt) ->
                  let bid = bookmarkTagBookmarkId bt
                      tag = bookmarkTagTag bt
                      seq' = bookmarkTagSeq bt
                   in Map.insertWith
                        (\(newSeq, newTags) (oldSeq, oldTags) -> (max newSeq oldSeq, Set.union newTags oldTags))
                        bid
                        (seq', Set.singleton (CI.mk tag))
                        m
              )
              Map.empty
              existingTags
          newEntries =
            kbids >>= \kbid ->
              let (maxSeq, existingSet) = Map.findWithDefault (0, Set.empty) kbid bidTagMap
                  newTags = filter (`Set.notMember` existingSet) (map CI.mk addTags')
               in zipWith (\i tag -> BookmarkTag userId tag kbid i) [maxSeq + 1 ..] (map CI.original newTags)
      insertMany_ newEntries

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
    <$> [ "%FT%T%Q%Z", -- 2018-12-31T23:59:59.123Z  (iso8601 with timezone)
          "%FT%T%Q", -- 2018-12-31T23:59:59.123   (iso8601 without timezone)
          "%F %T%Q", -- 2018-12-31 23:59:59.123
          "%F %T%Q %Z", -- 2018-12-31 23:59:59.123 UTC
          "%F", -- 2018-12-31
          "%-m/%-d/%Y", -- 12/31/2018
          "%s", -- 1535932800
          "%s%Q" -- 1535932800.123
        ]

withTags :: Key Bookmark -> DB [Entity BookmarkTag]
withTags key = selectList [BookmarkTagBookmarkId CP.==. key] [Asc BookmarkTagSeq]

-- Note List Query

getNote :: Key User -> NtSlug -> DB (Maybe (Entity Note))
getNote userKey slug =
  selectFirst [NoteUserId CP.==. userKey, NoteSlug CP.==. slug] []

getNoteList :: Key User -> Maybe Text -> Maybe NotePagingCursorTime -> SharedP -> Limit -> Page -> DB (Int, [Entity Note], Bool, Bool)
getNoteList key mquery mcursor sharedp limit' page = do
  total <-
    fmap
      (sum . fmap unValue)
      ( select $ do
          b <- from (table @Note)
          _whereClause b
          pure countRows
      )
  rows <-
    select $ do
      b <- from (table @Note)
      _whereClause b
      for_ mcursor $ \case
        PagingCursorBefore before -> where_ (isBeforeCursor b before)
        PagingCursorAfter after -> where_ (isAfterCursor b after)
      orderBy (noteOrder b)
      limit (limit' + 1) -- fetch one extra row to determine if there are more rows in the given direction
      offset ((page - 1) * limit')
      pure b
  let hasMoreInQueryDirection = fromIntegral (length rows) > limit'
      pageRows = take (fromIntegral limit') rows
      hasEarlier =
        case mcursor of
          Just (PagingCursorAfter _) -> not (null pageRows)
          _ -> hasMoreInQueryDirection
      hasLater =
        case mcursor of
          Just (PagingCursorAfter _) -> hasMoreInQueryDirection
          Just (PagingCursorBefore _) -> not (null pageRows)
          Nothing -> False
  pure (total, finalizeRows pageRows, hasEarlier, hasLater)
  where
    _whereClause b = do
      where_ (b ^. NoteUserId ==. val key)
      -- search
      sequenceA_ (parseSearchQuery (toLikeExpr b) =<< mquery)
      case sharedp of
        SharedAll -> pure ()
        SharedPublic -> where_ (b ^. NoteShared ==. val True)
        SharedPrivate -> where_ (b ^. NoteShared ==. val False)
    toLikeExpr b term = fromRight p_allFields (P.parseOnly p_onefield term)
      where
        wild s = (%) ++. val s ++. (%)
        toLikeN field s = b ^. field `like` wild s
        p_allFields = toLikeN NoteTitle term ||. toLikeN NoteText term
        p_onefield = p_title <|> p_text <|> p_after <|> p_before
          where
            p_title = ("title:" <|> "ti:") *> fmap (toLikeN NoteTitle) P.takeText
            p_text = ("description:" <|> "d:") *> fmap (toLikeN NoteText) P.takeText
            p_after = ("after:" <|> "a:") *> fmap ((b ^. NoteCreated >=.) . val) (parseTimeText =<< P.takeText)
            p_before = ("before:" <|> "b:") *> fmap ((b ^. NoteCreated <=.) . val) (parseTimeText =<< P.takeText)
    noteOrder b =
      case mcursor of
        Just (PagingCursorAfter _) -> [asc (b ^. NoteCreated), asc (b ^. NoteId)]
        _ -> [desc (b ^. NoteCreated), desc (b ^. NoteId)]
    finalizeRows =
      case mcursor of
        Just (PagingCursorAfter _) -> reverse -- because of asc order fetch for after cursor, reverse to get newest first order
        _ -> id
    isBeforeCursor b before =
      (b ^. NoteCreated) Database.Esqueleto.Experimental.<. val before
    isAfterCursor b after =
      (b ^. NoteCreated) Database.Esqueleto.Experimental.>. val after

mkBookmarkTags :: Key User -> Key Bookmark -> [Tag] -> [BookmarkTag]
mkBookmarkTags userId bookmarkId tags =
  (\(i, tag) -> BookmarkTag userId tag bookmarkId i) <$> zip [1 ..] tags

-- * Bookmark Tag Cloud

data TagCloudMode
  = TagCloudModeTop Bool -- { mode: "top" }
  | TagCloudModeTopLowerBound Bool Int -- { mode: "lowerBound", value: 20 }
  | TagCloudModeRelated Bool [Tag] -- { mode: "related", value: "tag1 tag2" }
  | TagCloudModeRelatedLowerBound Bool [Tag] Int -- { mode: "relatedLowerBound", value: "tag1 tag2", lowerBound: 5 }
  | TagCloudModeNone
  deriving (Show, Eq, Read, Generic)

isExpanded :: TagCloudMode -> Bool
isExpanded (TagCloudModeTop e) = e
isExpanded (TagCloudModeTopLowerBound e _) = e
isExpanded (TagCloudModeRelated e _) = e
isExpanded (TagCloudModeRelatedLowerBound e _ _) = e
isExpanded TagCloudModeNone = False

instance FromJSON TagCloudMode where
  parseJSON (Object o) =
    case KM.lookup "mode" o of
      Just (String "top") -> TagCloudModeTop <$> o .: "expanded"
      Just (String "lowerBound") -> TagCloudModeTopLowerBound <$> o .: "expanded" <*> o .: "value"
      Just (String "related") -> TagCloudModeRelated <$> o .: "expanded" <*> fmap words (o .: "value")
      Just (String "relatedLowerBound") -> TagCloudModeRelatedLowerBound <$> o .: "expanded" <*> fmap words (o .: "value") <*> o .: "lowerBound"
      Just (String "none") -> pure TagCloudModeNone
      _ -> A.parseFail "bad parse"
  parseJSON _ = A.parseFail "bad parse"

instance ToJSON TagCloudMode where
  toJSON (TagCloudModeTop e) =
    object
      [ "mode" .= String "top",
        "expanded" .= Bool e
      ]
  toJSON (TagCloudModeTopLowerBound e i) =
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
  toJSON (TagCloudModeRelatedLowerBound e tags lb) =
    object
      [ "mode" .= String "relatedLowerBound",
        "value" .= String (unwords tags),
        "lowerBound" .= toJSON lb,
        "expanded" .= Bool e
      ]
  toJSON TagCloudModeNone =
    object
      [ "mode" .= String "none",
        "value" .= Null,
        "expanded" .= Bool False
      ]

type Tag = Text

tagCountTop :: Key User -> DB [(Text, Int)]
tagCountTop user =
  sortOn (toLower . fst)
    . fmap (bimap unValue unValue)
    <$> ( select $ do
            t <- from (table @BookmarkTag)
            where_ (t ^. BookmarkTagUserId ==. val user)
            groupBy (lower_ $ t ^. BookmarkTagTag)
            let countRows' = countRows
            orderBy [desc countRows']
            limit 200
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
  sortOn (toLower . fst)
    . fmap (bimap unValue unValue)
    <$> ( select $ do
            t <- from (table @BookmarkTag)
            where_ (relatedTagWhere user tags t)
            groupBy (lower_ $ t ^. BookmarkTagTag)
            let countRows' = countRows
            orderBy [desc countRows']
            limit 200
            pure (t ^. BookmarkTagTag, countRows')
        )

tagCountRelatedLowerBound :: Key User -> [Tag] -> Int -> DB [(Text, Int)]
tagCountRelatedLowerBound user tags lowerBound =
  fmap (bimap unValue unValue)
    <$> ( select $ do
            t <- from (table @BookmarkTag)
            where_ (relatedTagWhere user tags t)
            groupBy (lower_ $ t ^. BookmarkTagTag)
            let countRows' = countRows
            having (countRows' >=. val lowerBound)
            orderBy [asc $ lower_ $ (t ^. BookmarkTagTag)]
            pure (t ^. BookmarkTagTag, countRows')
        )

relatedTagWhere :: Key User -> [Tag] -> SqlExpr (Entity BookmarkTag) -> SqlExpr (Value Bool)
relatedTagWhere user tags t =
  foldl
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
upsertBookmark userId _ bm _ | userId /= bookmarkUserId bm = pure (Failed "unauthorized")
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
      getBy (UniqueUserHref userId (bookmarkHref bm)) >>= \case
        Just (Entity bid prev_bm)
          | userId == bookmarkUserId prev_bm ->
              replaceBookmark bid prev_bm
        Just _ -> pure (Failed "unauthorized")
        _ -> Created <$> insert bm
  forM_ (maybeUpsertResult res) (insertTags (bookmarkUserId bm))
  pure res
  where
    prepareReplace prev_bm =
      if bookmarkHref bm /= bookmarkHref prev_bm
        then bm {bookmarkSlug = bookmarkSlug prev_bm, bookmarkArchiveHref = Nothing}
        else bm {bookmarkSlug = bookmarkSlug prev_bm, bookmarkArchiveHref = bookmarkArchiveHref prev_bm}
    replaceBookmark bid prev_bm = do
      replace bid (prepareReplace prev_bm)
      deleteTags bid
      pure (Updated bid)
    deleteTags bid =
      deleteWhere [BookmarkTagBookmarkId CP.==. bid]
    insertTags userId' bid' =
      insertMany_ (mkBookmarkTags userId' bid' tags)

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
