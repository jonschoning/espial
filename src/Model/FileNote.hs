module Model.FileNote where

import ClassyPrelude.Yesod hiding (Value, exists, groupBy, on, (<=.), (==.), (>=.), (||.))
import Control.Monad.Fail (MonadFail)
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A (parseFail)
import Database.Persist.Sql (fromSqlKey)
import Model
import Model.Custom
import System.Directory (listDirectory)
import Types

-- * FileNotes

data FileNote = FileNote
  { fileNoteId :: !Text,
    fileNoteSlug :: !(Maybe Text),
    fileNoteTitle :: !Text,
    fileNoteText :: !Text,
    fileNoteLength :: !Int,
    fileNoteIsMarkdown :: !(Maybe Bool),
    fileNoteShared :: !(Maybe Bool),
    fileNoteCreatedAt :: !UTCTime,
    fileNoteUpdatedAt :: !UTCTime
  }
  deriving (Show, Eq, Ord)

instance FromJSON FileNote where
  parseJSON (Object o) =
    FileNote
      <$> o
      .: "id"
      <*> o
      A..:? "slug"
      <*> o
      .: "title"
      <*> o
      .: "text"
      <*> o
      .: "length"
      <*> o
      A..:? "is_markdown"
      <*> o
      A..:? "shared"
      <*> (readFileNoteTime =<< o .: "created_at")
      <*> (readFileNoteTime =<< o .: "updated_at")
  parseJSON _ = A.parseFail "bad parse"

instance ToJSON FileNote where
  toJSON FileNote {..} =
    object
      [ "id" .= fileNoteId,
        "slug" .= fileNoteSlug,
        "title" .= fileNoteTitle,
        "text" .= fileNoteText,
        "length" .= fileNoteLength,
        "is_markdown" .= fileNoteIsMarkdown,
        "shared" .= fileNoteShared,
        "created_at" .= showFileNoteTime fileNoteCreatedAt,
        "updated_at" .= showFileNoteTime fileNoteUpdatedAt
      ]
  toEncoding FileNote {..} =
    A.pairs
      ( "id"
          .= fileNoteId
          <> "slug"
          .= fileNoteSlug
          <> "title"
          .= fileNoteTitle
          <> "text"
          .= fileNoteText
          <> "length"
          .= fileNoteLength
          <> "is_markdown"
          .= fileNoteIsMarkdown
          <> "shared"
          .= fileNoteShared
          <> "created_at"
          .= showFileNoteTime fileNoteCreatedAt
          <> "updated_at"
          .= showFileNoteTime fileNoteUpdatedAt
      )

readFileNoteTime ::
  (MonadFail m) =>
  String -> m UTCTime
readFileNoteTime = parseTimeM True defaultTimeLocale "%F %T"

showFileNoteTime :: UTCTime -> String
showFileNoteTime = formatTime defaultTimeLocale "%F %T"

fileNoteToNote :: UserId -> FileNote -> IO Note
fileNoteToNote user FileNote {..} = do
  slug <- mkNtSlug
  pure
    $ Note
      { noteUserId = user,
        noteSlug = slug,
        noteLength = fileNoteLength,
        noteTitle = fileNoteTitle,
        noteText = fileNoteText,
        noteIsMarkdown = fromMaybe False fileNoteIsMarkdown,
        noteShared = fromMaybe False fileNoteShared,
        noteCreated = fileNoteCreatedAt,
        noteUpdated = fileNoteUpdatedAt
      }

readDirFileNotes :: (MonadIO m) => FilePath -> m (Either String [FileNote])
readDirFileNotes fdir = liftIO $ do
  files <- listDirectory fdir
  noteBSS <- mapM (readFile . (fdir </>)) files
  pure (mapM (A.eitherDecode' . fromStrict) noteBSS)

readFileNotes :: (MonadIO m) => FilePath -> m (Either String [FileNote])
readFileNotes fpath =
  A.eitherDecode' . fromStrict <$> readFile fpath

insertFileNotes :: Key User -> [FileNote] -> DB Int
insertFileNotes userId fnotes = do
  notes <- liftIO $ mapM (fileNoteToNote userId) fnotes
  ins <- forM (zip fnotes notes) $ \(fnote, note) -> do
    existing <- case fileNoteSlug fnote of
      Just fslug -> checkUnique note {noteSlug = NtSlug fslug}
      Nothing -> pure Nothing
    case existing of
      Just _ -> pure 0
      Nothing -> insertUnique note >> pure 1
  pure (sum ins)

insertDirFileNotes :: Key User -> FilePath -> DB (Either String Int)
insertDirFileNotes userId noteDirectory = do
  mfnotes <- liftIO $ readDirFileNotes noteDirectory
  case mfnotes of
    Left e -> pure $ Left e
    Right fnotes -> Right <$> insertFileNotes userId fnotes

noteToFileNote :: Entity Note -> FileNote
noteToFileNote (Entity k Note {..}) =
  FileNote
    { fileNoteId = tshow (fromSqlKey k),
      fileNoteSlug = Just (unNtSlug noteSlug),
      fileNoteTitle = noteTitle,
      fileNoteText = noteText,
      fileNoteLength = noteLength,
      fileNoteIsMarkdown = Just noteIsMarkdown,
      fileNoteShared = Just noteShared,
      fileNoteCreatedAt = noteCreated,
      fileNoteUpdatedAt = noteUpdated
    }

getFileNotes :: Key User -> DB [FileNote]
getFileNotes user = fmap noteToFileNote <$> allUserNotes user

exportFileNotes :: Key User -> FilePath -> DB ()
exportFileNotes user fpath =
  liftIO . A.encodeFile fpath =<< getFileNotes user
