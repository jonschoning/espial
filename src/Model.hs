{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Model where

import ClassyPrelude.Yesod hiding (Value, delete, exists, groupBy, on, update, (<=.), (=.), (==.), (>=.), (||.))
import ClassyPrelude.Yesod qualified as CP
import Control.Monad (fail)
import Control.Monad.Combinators qualified as PC (between)
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
import Data.Text qualified as T
import Data.Text.Lazy.Builder qualified as TLB
import Database.Esqueleto.Experimental hiding ((<&>))
import Database.Esqueleto.Internal.Internal (unsafeSqlFunction, unsafeSqlValue)
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
    deriving Show Eq Ord

-- | List of Espial versions that have interacted with the database
AppVersion
    appVersionSpec Text
    appVersion Text
    appGitSha Text
    created UTCTime
    deriving Show Eq Ord

User json
  name Text
  passwordHash PasswordHash
  apiToken HashedApiKey Maybe
  privateDefault Bool
  archiveDefault Bool
  suggestTags Bool default=True
  suggestTagsUseReturnKey Bool default=True
  privacyLock Bool
  publicTagCloud Bool default=False
  previewNotes Bool default=True
  language I18nLang Maybe
  UniqueUserName name
  deriving Show Eq Ord

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
  deriving Show Eq Ord

BookmarkTag json
  userId UserId OnDeleteCascade
  tag Text
  bookmarkId BookmarkId OnDeleteCascade
  seq Int
  UniqueUserTagBookmarkId userId tag bookmarkId
  UniqueUserBookmarkIdTagSeq userId bookmarkId tag seq
  deriving Show Eq Ord

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
  UniqueUserNoteSlug userId slug
  deriving Show Eq Ord

-- | Durable record of a pending archive job
ArchiveJobRecord
  userId UserId OnDeleteCascade
  bookmarkId BookmarkId OnDeleteCascade
  href Text
  created UTCTime
  deriving Show Eq Ord
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

data SettingsTabP
  = SettingsTabImportExport
  | SettingsTabApi
  deriving (Eq, Show, Read)

-- * Paging

data PagingCursor a
  = PagingCursorBefore a
  | PagingCursorAfter a
  deriving (Eq, Show, Read)

type BookmarkPagingCursorTime = PagingCursor UTCTime

type NotePagingCursorTime = PagingCursor UTCTime

-- * Sorting

data SortDirection
  = SortAsc
  | SortDesc
  deriving (Eq, Show, Read)

data BookmarkSortField
  = BookmarkSortTime
  | BookmarkSortTitle
  | BookmarkSortNumTags
  | BookmarkSortUrl
  deriving (Eq, Show, Read)

data BookmarkSort = BookmarkSort BookmarkSortField SortDirection
  deriving (Eq, Show, Read)

-- | Cursor-based (before/after) paging is only defined for this ordering;
-- other sorts fall back to page/offset paging. See 'bookmarksTagsQuery'.
defaultBookmarkSort :: BookmarkSort
defaultBookmarkSort = BookmarkSort BookmarkSortTime SortDesc

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

-- | Case-insensitive exact tag match via SQLite's three-arg like(pattern, string, escape).
-- Equivalent to: col LIKE f v ESCAPE e
sqliteLike :: (Text -> Text) -> Text -> SqlExpr (Value Text) -> Text -> SqlExpr (Value Bool)
sqliteLike f e col v = unsafeSqlFunction "like" [val (f v), col, val e]

-- Equivalent to: col LIKE escapedTag ESCAPE '\'
sqliteLikeExact :: SqlExpr (Value Text) -> Text -> SqlExpr (Value Bool)
sqliteLikeExact col = sqliteLike escapeLike "\\" col

-- Equivalent to: col LIKE '%' || escaped_term || '%' ESCAPE '\'
sqliteLikeContains :: SqlExpr (Value Text) -> Text -> SqlExpr (Value Bool)
sqliteLikeContains col = sqliteLike (\v -> "%" <> escapeLike v <> "%") "\\" col

-- | Escape wildcards in a string for use in a LIKE pattern, so that _ and % match literally.
escapeLike :: Text -> Text
escapeLike = T.replace "_" "\\_" . T.replace "%" "\\%" . T.replace "\\" "\\\\"

authenticatePassword :: HashAlgoConfig -> Text -> Text -> DB (Maybe (Entity User))
authenticatePassword rehashAlgo username password = do
  getBy (UniqueUserName username) >>= \case
    Nothing -> pure Nothing
    Just entity@(Entity uid user) ->
      let stored = userPasswordHash user
       in if validatePasswordHash stored password
            then do
              when (needsRehash rehashAlgo stored) $ do
                newHash <- liftIO (hashPasswordWith rehashAlgo password)
                CP.update uid [UserPasswordHash CP.=. newHash]
              pure (Just entity)
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
  BookmarkSort ->
  Limit ->
  Page ->
  DB (Int, [(Entity Bookmark, Maybe Text)], Bool, Bool)
bookmarksTagsQuery userId isowner sharedp filterp tags mquery mcursor bsort limit' page = do
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
          for_ effectiveCursor $ \case
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
      -- sorts without cursor support always page by offset, where "earlier"
      -- and "later" just mean the previous/next page rather than carrying
      -- any temporal meaning
      hasEarlier
        | bsort /= defaultBookmarkSort = page > 1
        | otherwise =
            case effectiveCursor of
              Just (PagingCursorAfter _) -> not (null pageRows)
              _ -> hasMoreInQueryDirection
      hasLater
        | bsort /= defaultBookmarkSort = hasMoreInQueryDirection
        | otherwise =
            case effectiveCursor of
              Just (PagingCursorAfter _) -> hasMoreInQueryDirection
              Just (PagingCursorBefore _) -> not (null pageRows)
              Nothing -> False
  pure (total, finalizeRows pageRows, hasEarlier, hasLater)
  where
    -- before/after cursor paging assumes newest-first BookmarkTime order;
    -- any other sort falls back to page/offset paging instead
    effectiveCursor
      | bsort == defaultBookmarkSort = mcursor
      | otherwise = Nothing
    bookmarkOrder b = case bsort of
      BookmarkSort BookmarkSortTime SortDesc ->
        case effectiveCursor of
          Just (PagingCursorAfter _) -> [asc (b ^. BookmarkTime), asc (b ^. BookmarkId)]
          _ -> [desc (b ^. BookmarkTime), desc (b ^. BookmarkId)]
      BookmarkSort BookmarkSortTime SortAsc ->
        [asc (b ^. BookmarkTime), asc (b ^. BookmarkId)]
      BookmarkSort BookmarkSortTitle SortAsc ->
        [asc (lower_ (b ^. BookmarkDescription)), asc (b ^. BookmarkId)]
      BookmarkSort BookmarkSortTitle SortDesc ->
        [desc (lower_ (b ^. BookmarkDescription)), desc (b ^. BookmarkId)]
      BookmarkSort BookmarkSortNumTags SortAsc ->
        [asc (bookmarkNumTagsExpr isowner b), asc (b ^. BookmarkId)]
      BookmarkSort BookmarkSortNumTags SortDesc ->
        [desc (bookmarkNumTagsExpr isowner b), desc (b ^. BookmarkId)]
      BookmarkSort BookmarkSortUrl SortAsc ->
        [asc (bookmarkHrefNoSchemeExpr b), asc (b ^. BookmarkId)]
      BookmarkSort BookmarkSortUrl SortDesc ->
        [desc (bookmarkHrefNoSchemeExpr b), desc (b ^. BookmarkId)]
    finalizeRows = case effectiveCursor of
      Just (PagingCursorAfter _) -> reverse -- because of asc order fetch for after cursor, reverse to get newest first order
      _ -> id
    isBeforeCursor b before =
      ((b ^. BookmarkTime) Database.Esqueleto.Experimental.<. val before)
    isAfterCursor b after =
      ((b ^. BookmarkTime) Database.Esqueleto.Experimental.>. val after)

-- | Number of tags on a bookmark, for sorting by tag count. Excludes
-- dot-prefixed (private) tags for non-owners, matching the tag list built
-- alongside it in 'bookmarksTagsQuery'.
bookmarkNumTagsExpr :: Bool -> SqlExpr (Entity Bookmark) -> SqlExpr (Value (Maybe Int))
bookmarkNumTagsExpr isowner b =
  subSelect $ from (table @BookmarkTag) >>= \t -> do
    where_ (t ^. BookmarkTagBookmarkId ==. b ^. BookmarkId)
    when
      (not isowner)
      (where_ (not_ (t ^. BookmarkTagTag `like` val ".%")))
    pure countRows

-- | Bookmark href with a leading "<scheme>://" stripped, for sorting by URL
-- while ignoring scheme. The '://' delimiter and the 0/3 literals are
-- spliced as raw SQL (not bind parameters) so this expression matches,
-- token-for-token, the expression index idx_bookmark_url_no_scheme created
-- in Model.Migrations.operation_create_initial_indexes -- SQLite can only
-- use an expression index when the query expression is parsed identically
-- to the indexed expression, and bind parameters never match literals.
-- Keep both sides in sync if this expression ever changes.
bookmarkHrefNoSchemeExpr :: SqlExpr (Entity Bookmark) -> SqlExpr (Value Text)
bookmarkHrefNoSchemeExpr b =
  case_
    [when_ (schemeEnd Database.Esqueleto.Experimental.>. sqlIntLit 0) then_ (sqliteSubstr href (schemeEnd +. sqlIntLit 3))]
    (else_ href)
  where
    href = b ^. BookmarkHref
    schemeEnd = sqliteInstr href "://"
    sqlIntLit :: Int -> SqlExpr (Value Int)
    sqlIntLit n = unsafeSqlValue (TLB.fromString (show n))
    sqliteSubstr col start = unsafeSqlFunction "substr" (col, start)
    sqliteInstr col str = unsafeSqlFunction "instr" (col, sqlStrLit str)
      where
        sqlStrLit t = unsafeSqlValue ("'" <> TLB.fromText (T.replace "'" "''" t) <> "'")

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
                        &&. sqliteLikeExact (t ^. BookmarkTagTag) tag
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
    toLikeB field s = sqliteLikeContains (b ^. field) s
    toExactB field s = sqliteLikeExact (b ^. field) s
    tagsExpr match term' =
      exists $ from (table @BookmarkTag) >>= \t ->
        where_
          $ (t ^. BookmarkTagBookmarkId ==. b ^. BookmarkId)
          &&. match (t ^. BookmarkTagTag) term'
    p_allFields =
      toLikeB BookmarkHref term
        ||. toLikeB BookmarkDescription term
        ||. toLikeB BookmarkExtended term
        ||. tagsExpr sqliteLikeContains term
    p_onefield = p_url <|> p_title <|> p_description <|> p_tags <|> p_after <|> p_before
      where
        p_url = p_textField ("url" <|> "u") BookmarkHref
        p_title = p_textField ("title" <|> "ti") BookmarkDescription
        p_description = p_textField ("description" <|> "d") BookmarkExtended
        p_tags =
          ("tags" <|> "t")
            *> ( P.char ':'
                   *> fmap (tagsExpr sqliteLikeContains) P.takeText
                   <|> P.char '='
                   *> fmap (tagsExpr sqliteLikeExact) P.takeText
               )
        p_after = ("after:" <|> "a:") *> fmap ((b ^. BookmarkTime >=.) . val) (parseTimeText =<< P.takeText)
        p_before = ("before:" <|> "b:") *> fmap ((b ^. BookmarkTime <=.) . val) (parseTimeText =<< P.takeText)
        p_textField name field =
          name
            *> ( P.char ':'
                   *> fmap (toLikeB field) P.takeText
                   <|> P.char '='
                   *> fmap (toExactB field) P.takeText
               )

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

data BulkAction
  = BulkActionRead
  | BulkActionUnread
  | BulkActionStar
  | BulkActionUnstar
  | BulkActionDelete
  | BulkActionPrivate
  | BulkActionPublic
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
    Left err -> pure (Left err)
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
      pure (Right n)
  where
    toKbids = map (toSqlKey @Bookmark)

    validateCount = case _beSelection of
      BulkSelectionPage bids ->
        if length bids /= _beSelectionCount
          then pure $ Left BulkEditErrorPageMismatch
          else pure (Right (length bids))
      BulkSelectionAll fp sp ts q -> do
        result <-
          select $ from (table @Bookmark) >>= \b -> do
            bookmarkWhereClause userId sp fp ts q b
            pure countRows
        pure $ Right $ maybe 0 unValue (listToMaybe result)

    resolveKbids = case _beSelection of
      BulkSelectionPage bids -> pure (toKbids bids)
      BulkSelectionAll fp sp ts q ->
        fmap (map unValue)
          $ select
          $ from (table @Bookmark)
          >>= \b -> do
            bookmarkWhereClause userId sp fp ts q b
            pure (b ^. BookmarkId)

    applyAction = \case
      BulkActionPrivate -> bulkUpdate BookmarkShared False
      BulkActionPublic -> bulkUpdate BookmarkShared True
      BulkActionRead -> bulkUpdate BookmarkToRead False
      BulkActionStar -> bulkUpdate BookmarkSelected True
      BulkActionUnread -> bulkUpdate BookmarkToRead True
      BulkActionUnstar -> bulkUpdate BookmarkSelected False
      BulkActionDelete -> case _beSelection of
        BulkSelectionPage bids ->
          CP.deleteWhere [BookmarkId CP.<-. toKbids bids, BookmarkUserId CP.==. userId]
        BulkSelectionAll fp sp ts q ->
          delete $ from (table @Bookmark) >>= bookmarkWhereClause userId sp fp ts q

    bulkUpdate field v = case _beSelection of
      BulkSelectionPage bids ->
        CP.updateWhere [BookmarkId CP.<-. toKbids bids, BookmarkUserId CP.==. userId] [field CP.=. v]
      BulkSelectionAll fp sp ts q ->
        update $ \b -> do
          set b [field =. val v]
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
                  $ map (sqliteLikeExact (t ^. BookmarkTagTag)) removeTags
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

allUserNotes :: Key User -> DB [Entity Note]
allUserNotes user =
  select $ do
    n <- from (table @Note)
    where_ (n ^. NoteUserId ==. val user)
    orderBy [asc (n ^. NoteCreated)]
    pure n

parseSearchQuery ::
  (Text -> SqlExpr (Value Bool)) ->
  Text ->
  Maybe (SqlQuery ())
parseSearchQuery toExpr =
  fmap where_ . either (const Nothing) Just . P.parseOnly andE
  where
    andE = foldl1 (&&.) <$> P.many1 (P.skipSpace *> orE)
    orE = foldl1 (||.) <$> tokenTermE `P.sepBy1` P.char '|'
    tokenTermE = negE (groupE <|> termE) <|> groupE <|> termE
      where
        negE p = not_ <$> (P.char '-' *> p)
        groupE = P.char '(' *> andE <* P.skipSpace <* P.char ')'
        termE = toExpr <$> (fieldTerm <|> quotedTerm <|> simpleTerm)
        fieldTerm = concat <$> sequence [fieldName, P.string ":" <|> P.string "=", quotedTerm <|> simpleTerm]
        fieldName = P.takeWhile1 (\c -> not (isSpace c) && c /= ':' && c /= '=' && c /= '|' && c /= '(' && c /= ')')
        quotedTerm = PC.between (P.char '"') (P.char '"') (P.takeWhile1 (/= '"'))
        simpleTerm = P.takeWhile1 (\c -> not (isSpace c) && c /= ':' && c /= '|' && c /= '(' && c /= ')')

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
    _whereClause = noteWhereClause key sharedp mquery
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

noteWhereClause :: Key User -> SharedP -> Maybe Text -> SqlExpr (Entity Note) -> SqlQuery ()
noteWhereClause key sharedp mquery b = do
  where_ (b ^. NoteUserId ==. val key)
  -- search
  sequenceA_ (parseSearchQuery toLikeExpr =<< mquery)
  case sharedp of
    SharedAll -> pure ()
    SharedPublic -> where_ (b ^. NoteShared ==. val True)
    SharedPrivate -> where_ (b ^. NoteShared ==. val False)
  where
    toLikeExpr term = fromRight p_allFields (P.parseOnly p_onefield term)
      where
        toLikeN field s = sqliteLikeContains (b ^. field) s
        toExactN field s = sqliteLikeExact (b ^. field) s
        p_allFields = toLikeN NoteTitle term ||. toLikeN NoteText term
        p_onefield = p_title <|> p_text <|> p_markdown <|> p_after <|> p_before
          where
            p_title = p_textField ("title" <|> "ti") NoteTitle
            p_text = p_textField ("description" <|> "d") NoteText
            p_markdown = ("markdown:" <|> "m:") *> fmap ((b ^. NoteIsMarkdown ==.) . val) (parseBoolText =<< P.takeText)
            p_after = ("after:" <|> "a:") *> fmap ((b ^. NoteCreated >=.) . val) (parseTimeText =<< P.takeText)
            p_before = ("before:" <|> "b:") *> fmap ((b ^. NoteCreated <=.) . val) (parseTimeText =<< P.takeText)
            p_textField name field =
              name
                *> ( P.char ':'
                       *> fmap (toLikeN field) P.takeText
                       <|> P.char '='
                       *> fmap (toExactN field) P.takeText
                   )

-- * Note BulkEdit

data NoteBulkSelection
  = NoteBulkSelectionPage
      -- | selected note IDs on the current page
      [Int64]
  | NoteBulkSelectionAll
      -- | shared
      SharedP
      -- | query
      (Maybe Text)
  deriving (Show, Eq)

data NoteBulkAction
  = NoteBulkActionPrivate
  | NoteBulkActionPublic
  | NoteBulkActionDelete
  | NoteBulkActionMarkdown
  | NoteBulkActionPlaintext
  deriving (Show, Eq, Generic)

data NoteBulkEditForm = NoteBulkEditForm
  { _nbeSelection :: NoteBulkSelection,
    _nbeAction :: NoteBulkAction,
    _nbeSelectionCount :: Int
  }
  deriving (Show, Eq)

instance FromJSON NoteBulkAction where
  parseJSON = A.withText "NoteBulkAction" \case
    "private" -> pure NoteBulkActionPrivate
    "public" -> pure NoteBulkActionPublic
    "delete" -> pure NoteBulkActionDelete
    "markdown" -> pure NoteBulkActionMarkdown
    "plaintext" -> pure NoteBulkActionPlaintext
    _ -> empty

instance FromJSON NoteBulkEditForm where
  parseJSON = A.withObject "NoteBulkEditForm" \o -> do
    selTag <- o .: "selection"
    selection <- case (selTag :: Text) of
      "page" -> NoteBulkSelectionPage <$> o .: "nids"
      "all" -> NoteBulkSelectionAll <$> o .: "sharedp" <*> o A..:? "query"
      _ -> fail $ "Unknown selection: " <> unpack selTag
    NoteBulkEditForm selection
      <$> o
      .: "action"
      <*> o
      .: "selectionCount"

notesBulkEdit :: Key User -> NoteBulkEditForm -> DB (Either BulkEditError Int)
notesBulkEdit userId NoteBulkEditForm {..} = do
  eValid <- validateCount
  case eValid of
    Left err -> pure (Left err)
    Right n -> do
      applyAction _nbeAction
      pure (Right n)
  where
    toKnids = map (toSqlKey @Note)

    validateCount = case _nbeSelection of
      NoteBulkSelectionPage nids ->
        if length nids /= _nbeSelectionCount
          then pure $ Left BulkEditErrorPageMismatch
          else pure (Right (length nids))
      NoteBulkSelectionAll sp q -> do
        result <-
          select $ from (table @Note) >>= \n -> do
            noteWhereClause userId sp q n
            pure countRows
        pure $ Right $ maybe 0 unValue (listToMaybe result)

    applyAction = \case
      NoteBulkActionPrivate -> bulkUpdate NoteShared False
      NoteBulkActionPublic -> bulkUpdate NoteShared True
      NoteBulkActionMarkdown -> bulkUpdate NoteIsMarkdown True
      NoteBulkActionPlaintext -> bulkUpdate NoteIsMarkdown False
      NoteBulkActionDelete -> case _nbeSelection of
        NoteBulkSelectionPage nids ->
          CP.deleteWhere [NoteId CP.<-. toKnids nids, NoteUserId CP.==. userId]
        NoteBulkSelectionAll sp q ->
          delete $ from (table @Note) >>= noteWhereClause userId sp q

    bulkUpdate field v = case _nbeSelection of
      NoteBulkSelectionPage nids ->
        CP.updateWhere [NoteId CP.<-. toKnids nids, NoteUserId CP.==. userId] [field CP.=. v]
      NoteBulkSelectionAll sp q ->
        update $ \n -> do
          set n [field =. val v]
          noteWhereClause userId sp q n

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
  deriving (Show, Eq, Ord, Read, Generic)

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
    object ["mode" .= ("top" :: Text), "expanded" .= e]
  toJSON (TagCloudModeTopLowerBound e i) =
    object ["mode" .= ("lowerBound" :: Text), "value" .= i, "expanded" .= e]
  toJSON (TagCloudModeRelated e tags) =
    object ["mode" .= ("related" :: Text), "value" .= unwords tags, "expanded" .= e]
  toJSON (TagCloudModeRelatedLowerBound e tags lb) =
    object ["mode" .= ("relatedLowerBound" :: Text), "value" .= unwords tags, "lowerBound" .= lb, "expanded" .= e]
  toJSON TagCloudModeNone =
    object ["mode" .= ("none" :: Text), "value" .= A.Null, "expanded" .= False]
  toEncoding (TagCloudModeTop e) =
    A.pairs ("mode" .= ("top" :: Text) <> "expanded" .= e)
  toEncoding (TagCloudModeTopLowerBound e i) =
    A.pairs ("mode" .= ("lowerBound" :: Text) <> "value" .= i <> "expanded" .= e)
  toEncoding (TagCloudModeRelated e tags) =
    A.pairs ("mode" .= ("related" :: Text) <> "value" .= unwords tags <> "expanded" .= e)
  toEncoding (TagCloudModeRelatedLowerBound e tags lb) =
    A.pairs ("mode" .= ("relatedLowerBound" :: Text) <> "value" .= unwords tags <> "lowerBound" .= lb <> "expanded" .= e)
  toEncoding TagCloudModeNone =
    A.pairs ("mode" .= ("none" :: Text) <> "value" .= A.Null <> "expanded" .= False)

type Tag = Text

tagCountTop :: Key User -> Bool -> DB [(Text, Int)]
tagCountTop user isOwner =
  sortOn (toLower . fst)
    . fmap (bimap unValue unValue)
    <$> ( select $ do
            t <- from (table @BookmarkTag)
            where_ (t ^. BookmarkTagUserId ==. val user)
            when (not isOwner) $ do
              b <- from (table @Bookmark)
              where_ (b ^. BookmarkId ==. t ^. BookmarkTagBookmarkId &&. b ^. BookmarkShared ==. val True)
              where_ (not_ (t ^. BookmarkTagTag `like` val ".%"))
            groupBy (lower_ $ t ^. BookmarkTagTag)
            let countRows' = countRows
            orderBy [desc countRows']
            limit 200
            pure (t ^. BookmarkTagTag, countRows')
        )

tagCountLowerBound :: Key User -> Int -> Bool -> DB [(Text, Int)]
tagCountLowerBound user lowerBound isOwner =
  fmap (bimap unValue unValue)
    <$> ( select $ do
            t <- from (table @BookmarkTag)
            where_ (t ^. BookmarkTagUserId ==. val user)
            when (not isOwner) $ do
              b <- from (table @Bookmark)
              where_ (b ^. BookmarkId ==. t ^. BookmarkTagBookmarkId &&. b ^. BookmarkShared ==. val True)
              where_ (not_ (t ^. BookmarkTagTag `like` val ".%"))
            groupBy (lower_ $ t ^. BookmarkTagTag)
            let countRows' = countRows
            orderBy [asc (t ^. BookmarkTagTag)]
            having (countRows' >=. val lowerBound)
            pure (t ^. BookmarkTagTag, countRows')
        )

tagCountRelated :: Key User -> [Tag] -> Bool -> DB [(Text, Int)]
tagCountRelated user tags isOwner =
  let effectiveTags = if isOwner then tags else filter (not . T.isPrefixOf ".") tags
   in sortOn (toLower . fst)
        . fmap (bimap unValue unValue)
        <$> ( select $ do
                t <- from (table @BookmarkTag)
                where_ (relatedTagWhere user effectiveTags t)
                when (not isOwner) $ do
                  b <- from (table @Bookmark)
                  where_
                    ( b ^. BookmarkId ==. t ^. BookmarkTagBookmarkId
                        &&. b ^. BookmarkShared ==. val True
                    )
                  where_ (not_ (t ^. BookmarkTagTag `like` val ".%"))
                groupBy (lower_ $ t ^. BookmarkTagTag)
                let countRows' = countRows
                orderBy [desc countRows']
                limit 200
                pure (t ^. BookmarkTagTag, countRows')
            )

tagCountRelatedLowerBound :: Key User -> [Tag] -> Int -> Bool -> DB [(Text, Int)]
tagCountRelatedLowerBound user tags lowerBound isOwner =
  let effectiveTags = if isOwner then tags else filter (not . T.isPrefixOf ".") tags
   in fmap (bimap unValue unValue)
        <$> ( select $ do
                t <- from (table @BookmarkTag)
                where_ (relatedTagWhere user effectiveTags t)
                when (not isOwner) $ do
                  b <- from (table @Bookmark)
                  where_ (b ^. BookmarkId ==. t ^. BookmarkTagBookmarkId &&. b ^. BookmarkShared ==. val True)
                  where_ (not_ (t ^. BookmarkTagTag `like` val ".%"))
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
                where_ (u ^. BookmarkTagBookmarkId ==. t ^. BookmarkTagBookmarkId &&. sqliteLikeExact (u ^. BookmarkTagTag) tag)
            )
    )
    (t ^. BookmarkTagUserId ==. val user)
    tags

fetchBookmarkByUrl :: Key User -> Maybe Text -> DB (Maybe (Entity Bookmark, [Entity BookmarkTag]))
fetchBookmarkByUrl userId murl = runMaybeT do
  bmark <- MaybeT . getBy . UniqueUserHref userId =<< MaybeT (pure murl)
  btags <- lift $ withTags (entityKey bmark)
  pure (bmark, btags)

data FailedReason
  = ReasonUnauthorized
  | ReasonNotFound
  | ReasonConflictWithNewer
  | ReasonHrefUsedByOther
  | ReasonInvalidInput Text
  deriving (Show, Eq)

data UpsertResult a = Created a | Updated a | Failed FailedReason
  deriving (Show, Eq, Functor)

upsertBookmark :: Key User -> Maybe (Key Bookmark) -> Bookmark -> [Text] -> DB (UpsertResult (Key Bookmark))
upsertBookmark userId _ bm _ | userId /= bookmarkUserId bm = pure (Failed ReasonUnauthorized)
upsertBookmark userId mbid bm tags = do
  res <- case mbid of
    Just bid ->
      get bid >>= \case
        Nothing -> pure (Failed ReasonNotFound)
        Just prev_bm | userId /= bookmarkUserId prev_bm -> pure (Failed ReasonUnauthorized)
        Just prev_bm -> do
          getBy (UniqueUserHref userId (bookmarkHref bm)) >>= \case
            Just (Entity otherBid _) | otherBid /= bid -> pure (Failed ReasonHrefUsedByOther)
            _ -> replaceBookmark bid prev_bm
    Nothing ->
      getBy (UniqueUserHref userId (bookmarkHref bm)) >>= \case
        Nothing -> Created <$> insert bm
        Just (Entity _ prev_bm) | userId /= bookmarkUserId prev_bm -> pure (Failed ReasonUnauthorized)
        Just (Entity bid prev_bm) -> replaceBookmark bid prev_bm
  forM_ (maybeUpsertResult res) (insertTags (bookmarkUserId bm))
  pure res
  where
    maybeUpsertResult (Created a) = Just a
    maybeUpsertResult (Updated a) = Just a
    maybeUpsertResult _ = Nothing
    replaceBookmark bid prev_bm = do
      replace bid (prepareBookmarkReplace prev_bm)
      deleteTags bid
      pure (Updated bid)
    deleteTags bid =
      deleteWhere [BookmarkTagBookmarkId CP.==. bid]
    insertTags userId' bid' =
      insertMany_ (mkBookmarkTags userId' bid' tags)
    -- \| Preserves the existing bookmark's slug (and, unless the href changed, its archive url).
    prepareBookmarkReplace :: Bookmark -> Bookmark
    prepareBookmarkReplace prev_bm =
      if bookmarkHref bm /= bookmarkHref prev_bm
        then bm {bookmarkSlug = bookmarkSlug prev_bm, bookmarkArchiveHref = Nothing}
        else bm {bookmarkSlug = bookmarkSlug prev_bm, bookmarkArchiveHref = bookmarkArchiveHref prev_bm}

upsertBookmarks :: Key User -> [Maybe (Key Bookmark)] -> [Bookmark] -> [[Text]] -> DB [UpsertResult (Key Bookmark)]
upsertBookmarks userId mbids bms tagss =
  forM (zip3 mbids bms tagss) $ \(mbid, bm, tags) -> upsertBookmark userId mbid bm tags

updateBookmarkArchiveUrl :: Key User -> Key Bookmark -> Maybe Text -> DB ()
updateBookmarkArchiveUrl userId bid marchiveUrl =
  updateWhere
    [BookmarkUserId CP.==. userId, BookmarkId CP.==. bid]
    [BookmarkArchiveHref CP.=. marchiveUrl]

upsertNote :: Key User -> Maybe (Key Note) -> Note -> DB (UpsertResult (Entity Note))
upsertNote userId mnid note = do
  case mnid of
    Nothing -> do
      now <- liftIO getCurrentTime
      let note'' = note {noteUserId = userId, noteLength = T.length (noteText note), noteUpdated = now}
      nid <- insert note''
      pure (Created (Entity nid note''))
    Just nid ->
      get nid >>= \case
        Nothing -> pure (Failed ReasonNotFound)
        Just note' | userId /= noteUserId note' -> pure (Failed ReasonUnauthorized)
        Just note' | noteUpdated note' > noteUpdated note -> pure (Failed ReasonConflictWithNewer)
        Just _ -> do
          now <- liftIO getCurrentTime
          let note'' = note {noteLength = T.length (noteText note), noteUpdated = now}
          replace nid note''
          pure (Updated (Entity nid note''))

-- * Archive Job Store

insertArchiveJobRecords :: [(Key User, Key Bookmark, Text)] -> DB [Key ArchiveJobRecord]
insertArchiveJobRecords records = do
  now <- liftIO getCurrentTime
  forM records $ \(userId, bid, href) -> insert (ArchiveJobRecord userId bid href now)

deleteArchiveJobRecord :: Key ArchiveJobRecord -> DB ()
deleteArchiveJobRecord jobId = CP.delete jobId

getArchiveJobRecords :: DB [Entity ArchiveJobRecord]
getArchiveJobRecords = selectList [] [Asc ArchiveJobRecordId]
