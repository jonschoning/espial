module Model.FileNote where

import ClassyPrelude.Yesod hiding (Value, exists, groupBy, on, (<=.), (==.), (>=.), (||.))
import Control.Monad.Fail (MonadFail)
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A (parseFail)
import Model
import Model.Custom
import System.Directory (listDirectory)
import Types

-- * FileNotes

data FileNote = FileNote
  { fileNoteId :: !Text,
    fileNoteTitle :: !Text,
    fileNoteText :: !Text,
    fileNoteLength :: !Int,
    fileNoteCreatedAt :: !UTCTime,
    fileNoteUpdatedAt :: !UTCTime
  }
  deriving (Show, Eq, Typeable, Ord)

instance FromJSON FileNote where
  parseJSON (Object o) =
    FileNote
      <$> o
      .: "id"
      <*> o
      .: "title"
      <*> o
      .: "text"
      <*> o
      .: "length"
      <*> (readFileNoteTime =<< o .: "created_at")
      <*> (readFileNoteTime =<< o .: "updated_at")
  parseJSON _ = A.parseFail "bad parse"

instance ToJSON FileNote where
  toJSON FileNote {..} =
    object
      [ "id" .= fileNoteId,
        "title" .= fileNoteTitle,
        "text" .= fileNoteText,
        "length" .= fileNoteLength,
        "created_at" .= showFileNoteTime fileNoteCreatedAt,
        "updated_at" .= showFileNoteTime fileNoteUpdatedAt
      ]
  toEncoding FileNote {..} =
    A.pairs
      ( "id" .= fileNoteId
      <> "title" .= fileNoteTitle
      <> "text" .= fileNoteText
      <> "length" .= fileNoteLength
      <> "created_at" .= showFileNoteTime fileNoteCreatedAt
      <> "updated_at" .= showFileNoteTime fileNoteUpdatedAt
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
        noteIsMarkdown = False,
        noteShared = False,
        noteCreated = fileNoteCreatedAt,
        noteUpdated = fileNoteUpdatedAt
      }

readDirFileNotes :: (MonadIO m) => FilePath -> m (Either String [FileNote])
readDirFileNotes fdir = liftIO $ do
  files <- listDirectory fdir
  noteBSS <- mapM (readFile . (fdir </>)) files
  pure (mapM (A.eitherDecode' . fromStrict) noteBSS)

insertFileNotes :: Key User -> [FileNote] -> DB Int
insertFileNotes userId fnotes = do
  notes <- liftIO $ mapM (fileNoteToNote userId) fnotes
  void $ mapM insertUnique notes
  pure (length notes)

insertDirFileNotes :: Key User -> FilePath -> DB (Either String Int)
insertDirFileNotes userId noteDirectory = do
  mfnotes <- liftIO $ readDirFileNotes noteDirectory
  case mfnotes of
    Left e -> pure $ Left e
    Right fnotes -> Right <$> insertFileNotes userId fnotes
