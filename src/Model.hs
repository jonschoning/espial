{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Model where

import qualified Data.Aeson as A

import Control.Monad.Writer hiding ((<>), mapM_)

import Types
import ModelCrypto
import Pretty
import ClassyPrelude.Yesod

import Control.Monad.Trans.Maybe
import Database.Esqueleto hiding ((==.))
import qualified Database.Esqueleto as E

-- Physical model

share [mkPersist sqlSettings, mkDeleteCascade sqlSettings, mkMigrate "migrateSchema"] [persistLowerCase| 
User
  name Text
  passwordHash BCrypt
  apiToken Text Maybe
  UniqueUserName name
  deriving Show Eq Typeable Ord

Bookmark
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

BookmarkTag
  userId UserId
  tag Text
  bookmarkId BookmarkId
  seq Int
  UniqueUserTagBookmarkId userId tag bookmarkId
  UniqueUserBookmarkIdTagSeq userId bookmarkId tag seq
  deriving Show Eq Typeable Ord
|]


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
    ]


-- User

authenticatePW :: Text -> Text -> DB (Maybe (Entity User))
authenticatePW username password = do
   muser <- getBy (UniqueUserName username)
   case muser of
     Nothing -> pure Nothing
     Just e@(Entity _ user) -> 
        if passwordMatches (userPasswordHash user) (password)
          then pure $ Just e
          else pure $ Nothing

getUserByName :: UserNameP -> DB (Maybe (Entity User))
getUserByName (UserNameP uname) =
  return . headMay =<<
  (select $
   from $ \u -> do
   where_ (u ^. UserName E.==. val uname)
   pure u)

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
      _inner b
      pure $ E.countRows)
      -- paged data
  <*> (select $
       from $ \b -> do
       _inner b
       orderBy [desc (b ^. BookmarkTime)]
       limit limit'
       offset ((page - 1) * limit')
       pure b)
  where
    _inner b = do
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

-- Bookmark Files

bookmarkEntityToTags :: Entity Bookmark -> [Tag] -> [BookmarkTag]
bookmarkEntityToTags (Entity {entityKey = bookmarkId
                             ,entityVal = Bookmark {..}}) tags =
  fmap
    (\(i, tag) -> BookmarkTag bookmarkUserId tag bookmarkId i)
    (zip [1 ..] tags)


postToBookmark :: UserId -> Post -> Bookmark
postToBookmark user Post {..} =
  Bookmark user postHref postDescription postExtended postTime postShared postToRead False


insertFileBookmarks :: Key User -> FilePath -> DB ()
insertFileBookmarks userId bookmarkFile = do
  posts' <- liftIO $ readBookmarkFileJson bookmarkFile
  case posts' of
      Left e -> print e
      Right posts -> do
        void $ do
            let bookmarks = fmap (postToBookmark userId) posts
            bookmarkIds <- insertMany bookmarks
            insertMany_ $ concatMap (uncurry bookmarkEntityToTags)
                (zipWith3 (\k v p -> (Entity k v, postTags p)) bookmarkIds bookmarks posts)
  where
    readBookmarkFileJson :: MonadIO m => FilePath -> m (Either String [Post])
    readBookmarkFileJson fpath = pure . A.eitherDecode' . fromStrict =<< readFile fpath

type Tag = Text

data Post = Post
  { postHref :: !Text
  , postDescription :: !Text
  , postExtended :: !Text
  , postTime :: !UTCTime
  , postShared :: !Bool
  , postToRead :: !Bool
  , postTags :: [Tag]
  } deriving (Show, Eq, Ord, Typeable)

instance FromJSON Post where
  parseJSON (Object o) =
    Post <$> o .: "href" <*> o .: "description" <*> o .: "extended" <*>
    o .: "time" <*>
    (boolFromYesNo <$> o .: "shared") <*>
    (boolFromYesNo <$> o .: "toread") <*>
    (words <$> o .: "tags")
  parseJSON _ = fail "bad parse"

instance ToJSON Post where
  toJSON Post {..} =
    object
      [ "href" .= toJSON postHref
      , "description" .= toJSON postDescription
      , "extended" .= toJSON postExtended
      , "time" .= toJSON postTime
      , "shared" .= boolToYesNo postShared
      , "toread" .= boolToYesNo postToRead
      , "tags" .= unwords postTags
      ]

boolFromYesNo :: Text -> Bool
boolFromYesNo "yes" = True
boolFromYesNo _ = False

boolToYesNo :: Bool -> Text
boolToYesNo True = "yes"
boolToYesNo _ = "no"

