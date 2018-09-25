{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Model where

import qualified Data.Aeson as A

import Control.Monad.Writer (tell, appEndo, Endo(..))

import Types
import ModelCrypto
import Pretty
import ClassyPrelude.Yesod
import System.Directory

import Control.Monad.Trans.Maybe
import Database.Esqueleto hiding ((==.))
import qualified Database.Esqueleto as E
import qualified Data.Time.ISO8601 as TI
import qualified Pinboard as PB

-- Physical model

share [mkPersist sqlSettings, mkDeleteCascade sqlSettings, mkMigrate "migrateSchema"] [persistLowerCase| 
User json
  name Text
  passwordHash BCrypt
  apiToken Text Maybe
  UniqueUserName name
  deriving Show Eq Typeable Ord

Bookmark json
  userId UserId
  href Text
  description Text
  extended Text
  time UTCTime
  shared Bool
  toRead Bool
  selected Bool
  UniqueUserHref userId href
  deriving Show Eq Typeable Ord

BookmarkTag json
  userId UserId
  tag Text
  bookmarkId BookmarkId
  seq Int
  UniqueUserTagBookmarkId userId tag bookmarkId
  UniqueUserBookmarkIdTagSeq userId bookmarkId tag seq
  deriving Show Eq Typeable Ord

Note json
  userId UserId
  length Int
  title Text
  text Text
  created UTCTime
  updated UTCTime
  deriving Show Eq Typeable Ord
|]

-- UTCTimeStr
  
newtype UTCTimeStr =
  UTCTimeStr { unUTCTimeStr :: UTCTime }
  deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

instance PathPiece UTCTimeStr where
  toPathPiece (UTCTimeStr u) = pack (TI.formatISO8601Millis u)
  fromPathPiece s = UTCTimeStr <$> TI.parseISO8601 (unpack s)


-- newtypes

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
  | FilterSingle Int64
  deriving (Eq, Show, Read)

newtype UnreadOnly =
  UnreadOnly { unUnreadOnly :: Bool }
  deriving (Eq, Show, Read)

type Limit = Int64
type Page = Int64

-- Migration

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



  

-- User

authenticatePassword :: Text -> Text -> DB (Maybe (Entity User))
authenticatePassword username password = do
  muser <- getBy (UniqueUserName username)
  case muser of
    Nothing -> return Nothing
    Just dbuser ->
      if validatePasswordHash (userPasswordHash (entityVal dbuser)) password
        then return (Just dbuser)
        else return Nothing




  
getUserByName :: UserNameP -> DB (Maybe (Entity User))
getUserByName (UserNameP uname) = do
  selectFirst [UserName ==. uname] []

-- bookmarks

bookmarksQuery
  :: Key User
  -> SharedP
  -> FilterP
  -> [Tag]
  -> Limit
  -> Page
  -> DB (Int, [Entity Bookmark])
bookmarksQuery userId sharedp filterp tags limit' page =
  (,) -- total count
  <$> fmap (sum . fmap E.unValue)
      (select $
      from $ \b -> do
      _whereClause b
      pure $ E.countRows)
      -- paged data
  <*> (select $
       from $ \b -> do
       _whereClause b
       orderBy [desc (b ^. BookmarkTime)]
       limit limit'
       offset ((page - 1) * limit')
       pure b)
  where
    _whereClause b = do
      where_ $
        foldMap (\tag -> Endo $ (&&.) 
          (exists $   -- each tag becomes an exists constraint
           from $ \t ->
           where_ (t ^. BookmarkTagBookmarkId E.==. b ^. BookmarkId &&.
                  (t ^. BookmarkTagTag E.==. val tag)))) tags
        `appEndo` (b ^. BookmarkUserId E.==. val userId)
      case sharedp of
        SharedAll -> pure ()
        SharedPublic ->  where_ (b ^. BookmarkShared E.==. val True)
        SharedPrivate -> where_ (b ^. BookmarkShared E.==. val False)
      case filterp of
        FilterAll -> pure ()
        FilterUnread ->   where_ (b ^. BookmarkToRead E.==. val True)
        FilterStarred ->  where_ (b ^. BookmarkSelected E.==. val True)
        FilterSingle i -> where_ (b ^. BookmarkId E.==. val (toSqlKey i))
        FilterUntagged -> where_ $ notExists $ from (\t -> where_ $ t ^. BookmarkTagBookmarkId E.==. b ^. BookmarkId)

tagsQuery :: [Entity Bookmark] -> DB [Entity BookmarkTag]
tagsQuery bmarks =
  select $
  from $ \t -> do
  where_ (t ^. BookmarkTagBookmarkId `in_` valList (fmap entityKey bmarks))
  orderBy [asc (t ^. BookmarkTagSeq)]
  pure t

withTags :: Key Bookmark -> DB [Entity BookmarkTag]
withTags key = selectList [BookmarkTagBookmarkId ==. key] [Asc BookmarkTagSeq]

-- Note List Query

  
getNote :: Key User -> Int64 -> DB (Maybe (Entity Note))
getNote userKey nid =
  selectFirst [NoteUserId ==. userKey, NoteId ==. toSqlKey nid] []

getNoteList :: Key User -> Limit -> Page -> DB (Int, [Entity Note])
getNoteList key limit' page =
  (,) -- total count
  <$> fmap (sum . fmap E.unValue)
      (select $
      from $ \b -> do
      where_ (b ^. NoteUserId E.==. val key)
      pure $ E.countRows)
  <*> (select $
       from $ \b -> do
       where_ (b ^. NoteUserId E.==. val key)
       orderBy [desc (b ^. NoteCreated)]
       limit limit'
       offset ((page - 1) * limit')
       pure b)

-- Bookmark Files

bookmarkEntityToTags :: Entity Bookmark -> [Tag] -> [BookmarkTag]
bookmarkEntityToTags (Entity {entityKey = bookmarkId
                             ,entityVal = Bookmark {..}}) tags =
  fmap
    (\(i, tag) -> BookmarkTag bookmarkUserId tag bookmarkId i)
    (zip [1 ..] tags)


pbPostToBookmark :: UserId -> PB.Post -> Bookmark
pbPostToBookmark user PB.Post {..} =
  Bookmark user postHref postDescription postExtended postTime postShared postToRead False


insertFilePbPosts :: Key User -> FilePath -> DB ()
insertFilePbPosts userId bookmarkFile = do
  posts' <- liftIO $ readPbPostFileJson bookmarkFile
  case posts' of
      Left e -> print e
      Right posts -> do
        let bookmarks = fmap (pbPostToBookmark userId) posts
        mbookmarkIds <- mapM insertUnique bookmarks 

        let bookmarkTags =
              concatMap (uncurry bookmarkEntityToTags) $
              catMaybes $
              zipWith3 (\mk v p -> map (\k -> (Entity k v, PB.postTags p)) mk)
                mbookmarkIds
                bookmarks
                posts
        void $ mapM insertUnique bookmarkTags
  where
    readPbPostFileJson :: MonadIO m => FilePath -> m (Either String [PB.Post])
    readPbPostFileJson fpath = pure . A.eitherDecode' . fromStrict =<< readFile fpath

type Tag = Text

-- Notes

pbNoteToNote :: UserId -> PB.Note -> Note
pbNoteToNote user PB.Note {..} =
  Note user noteLength noteTitle noteText noteCreatedAt noteUpdatedAt

insertDirPbNotes :: Key User -> FilePath -> DB ()
insertDirPbNotes userId noteDirectory = do
  pbnotes' <- liftIO $ readPbNoteFileJson noteDirectory
  case pbnotes' of
      Left e -> print e
      Right pbnotes -> do
        let notes = fmap (pbNoteToNote userId) pbnotes
        void $ mapM insertUnique notes 
  where
    readPbNoteFileJson :: MonadIO m => FilePath -> m (Either String [PB.Note])
    readPbNoteFileJson fdir = do
      files <- liftIO (listDirectory fdir)
      noteBSS <- mapM (readFile . (fdir </>)) files 
      pure (mapM (A.eitherDecode' . fromStrict) noteBSS) 

-- BookmarkForm

data BookmarkForm = BookmarkForm
  { _url :: Text
  , _title :: Maybe Text
  , _description :: Maybe Textarea
  , _tags :: Maybe Text
  , _private :: Maybe Bool
  , _toread :: Maybe Bool
  , _bid :: Maybe Int64
  , _selected :: Maybe Bool
  , _time :: Maybe UTCTimeStr
  } deriving (Show, Eq, Read, Generic)

instance FromJSON BookmarkForm where parseJSON = A.genericParseJSON gBookmarkFormOptions
instance ToJSON BookmarkForm where toJSON = A.genericToJSON gBookmarkFormOptions

gBookmarkFormOptions :: A.Options
gBookmarkFormOptions = A.defaultOptions { A.fieldLabelModifier = drop 1 } 

toBookmarkFormList :: [Entity Bookmark] -> [Entity BookmarkTag] -> [BookmarkForm]
toBookmarkFormList bs as = do
  b <- bs
  let bid = E.entityKey b
  let btags = filter ((==) bid . bookmarkTagBookmarkId . E.entityVal) as
  pure $ _toBookmarkForm (b, btags)

_toBookmarkForm :: (Entity Bookmark, [Entity BookmarkTag]) -> BookmarkForm
_toBookmarkForm (Entity bid Bookmark {..}, tags) =
  BookmarkForm
  { _url = bookmarkHref
  , _title = Just bookmarkDescription
  , _description = Just $ Textarea $ bookmarkExtended
  , _tags = Just $ unwords $ fmap (bookmarkTagTag . entityVal) tags
  , _private = Just $ not bookmarkShared
  , _toread = Just $ bookmarkToRead
  , _bid = Just $ fromSqlKey $ bid
  , _selected = Just $ bookmarkSelected
  , _time = Just $ UTCTimeStr $ bookmarkTime
  }

_toBookmark :: UserId -> UTCTime -> BookmarkForm -> Bookmark
_toBookmark userId time BookmarkForm {..} =
  Bookmark userId _url
    (fromMaybe "" _title)
    (maybe "" unTextarea _description)
    (fromMaybe time (fmap unUTCTimeStr _time))
    (maybe True not _private)
    (fromMaybe False _toread)
    (fromMaybe False _selected)

fetchBookmarkByUrl :: Key User -> Maybe Text -> DB (Maybe (Entity Bookmark, [Entity BookmarkTag]))
fetchBookmarkByUrl userId murl = runMaybeT $ do
  bmark <- MaybeT . getBy . UniqueUserHref userId =<< (MaybeT $ pure murl)
  btags <- lift $ withTags (entityKey bmark)
  pure (bmark, btags)

data UpsertResult = Created | Updated

upsertBookmark:: Maybe (Key Bookmark) -> Bookmark -> [Text] -> DB (UpsertResult, Key Bookmark)
upsertBookmark mbid bmark@Bookmark{..} tags = do
  res <- case mbid of
    Just bid -> do
      get bid >>= \case 
        Just _ -> replaceBookmark bid
        _ -> fail "not found"
    Nothing -> do
      getBy (UniqueUserHref bookmarkUserId bookmarkHref) >>= \case
        Just (Entity bid _) -> replaceBookmark bid
        _ -> (Created,) <$> insert bmark
  insertTags bookmarkUserId (snd res)
  pure res
  where 
    replaceBookmark bid = do
      replace bid bmark
      deleteTags bid
      pure (Updated, bid)
    deleteTags bid =
      deleteWhere [BookmarkTagBookmarkId ==. bid]
    insertTags userId bid' =
      forM_ (zip [1 ..] tags) $
      \(i, tag) -> void $ insert $ BookmarkTag userId tag bid' i


upsertNote:: Maybe (Key Note) -> Note -> DB (UpsertResult, Key Note)
upsertNote mnid bmark@Note{..} = do
  case mnid of
    Just nid -> do
      get nid >>= \case 
        Just _ -> do
          replace nid bmark
          pure (Updated, nid)
        _ -> fail "not found"
    Nothing -> do
      (Created,) <$> insert bmark
