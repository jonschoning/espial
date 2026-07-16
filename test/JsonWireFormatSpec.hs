{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Pins the exact JSON wire format sent to / read from
-- frontend/src/types.ts.
module JsonWireFormatSpec (spec) where

import Data.Aeson qualified as A
import Database.Persist.Sql (toSqlKey)
import Handler.Notes (NoteForm (..))
import Model.Custom (BmSlug (..), NtSlug (..), PasswordHash (..))
import Model.Form
import TestImport
import Yesod.Form.Fields (Textarea (..))

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2024 1 1) 0

t1 :: UTCTime
t1 = UTCTime (fromGregorian 2024 1 2) 0

shouldEncodeTo :: (A.ToJSON a) => a -> ByteString -> Expectation
shouldEncodeTo actual expectedJson =
  A.toJSON actual `shouldBe` fromMaybe (error "invalid expected JSON") (A.decodeStrict' expectedJson)

shouldDecodeTo :: (A.FromJSON a, Eq a, Show a) => ByteString -> a -> Expectation
shouldDecodeTo json expected = A.decodeStrict' json `shouldBe` Just expected

spec :: Spec
spec = describe "JSON wire format" $ do
  describe "BookmarkForm (app.dat.bmark / app.dat.bmarks)" $ do
    it "encodes every field, including archive fields, for an owned bookmark"
      $ bookmarkFormFixture
      `shouldEncodeTo` "{\"url\":\"https://example.com\",\"title\":\"Example\",\"description\":\"An extended description\",\"tags\":\"foo bar\",\"private\":false,\"toread\":true,\"bid\":1,\"slug\":\"abc123\",\"selected\":true,\"time\":\"2024-01-01T00:00:00Z\",\"archiveUrl\":\"https://archive.example.com/x\",\"archiveRequested\":true}"

    it "encodes unset optional fields as null, and omits archive fields entirely when absent"
      $ mkNewBookmarkForm False testUser "https://x.com" Nothing Nothing Nothing Nothing Nothing
      `shouldEncodeTo` "{\"url\":\"https://x.com\",\"title\":null,\"description\":null,\"tags\":null,\"private\":null,\"toread\":null,\"bid\":null,\"slug\":null,\"selected\":null,\"time\":null}"

    it "decodes a frontend-submitted bookmark form"
      $ "{\"url\":\"https://example.com\",\"title\":\"Example\",\"description\":\"An extended description\",\"tags\":\"foo bar\",\"private\":false,\"toread\":true,\"bid\":1,\"slug\":\"abc123\",\"selected\":true,\"time\":\"2024-01-01T00:00:00Z\",\"archiveUrl\":\"https://archive.example.com/x\",\"archiveRequested\":true}"
      `shouldDecodeTo` bookmarkFormFixture

  describe "Entity Note (app.dat.note / app.dat.notes)"
    $ it "encodes id, userId, and every Note field"
    $ noteFixture
    `shouldEncodeTo` "{\"id\":42,\"userId\":7,\"slug\":\"abcdefghij\",\"length\":4,\"title\":\"Title\",\"text\":\"Body text\",\"isMarkdown\":true,\"shared\":false,\"created\":\"2024-01-01T00:00:00Z\",\"updated\":\"2024-01-02T00:00:00Z\"}"

  describe "NoteForm (postAddNoteR request body)"
    $ it "decodes a frontend-submitted note form"
    $ "{\"id\":42,\"slug\":\"abcdefghij\",\"title\":\"Title\",\"text\":\"Body text\",\"isMarkdown\":true,\"shared\":false,\"created\":\"2024-01-01T00:00:00Z\",\"updated\":\"2024-01-02T00:00:00Z\"}"
    `shouldDecodeTo` NoteForm
      { _id = Just 42,
        _slug = Just (NtSlug "abcdefghij"),
        _title = Just "Title",
        _text = Just (Textarea "Body text"),
        _isMarkdown = Just True,
        _shared = Just False,
        _created = Just (UTCTimeStr t0),
        _updated = Just (UTCTimeStr t1)
      }

  describe "AccountSettingsForm (app.dat.accountSettings)"
    $ it "strips the leading underscore from every field name"
    $ AccountSettingsForm
      { _privateDefault = True,
        _archiveDefault = False,
        _suggestTags = True,
        _suggestTagsUseReturnKey = False,
        _privacyLock = False,
        _publicTagCloud = True,
        _previewNotes = True,
        _archiveBackendEnabled = False,
        _hasApiKey = True,
        _language = Just I18nLangDe
      }
    `shouldEncodeTo` "{\"privateDefault\":true,\"archiveDefault\":false,\"suggestTags\":true,\"suggestTagsUseReturnKey\":false,\"privacyLock\":false,\"publicTagCloud\":true,\"previewNotes\":true,\"archiveBackendEnabled\":false,\"hasApiKey\":true,\"language\":\"de\"}"

  describe "SharedP (app.dat.sharedp)"
    $ it "encodes as bare lowercase strings"
    $ do
      SharedAll `shouldEncodeTo` "\"all\""
      SharedPublic `shouldEncodeTo` "\"public\""
      SharedPrivate `shouldEncodeTo` "\"private\""

  describe "FilterP (app.dat.filter)"
    $ it "encodes as a {tag, contents} discriminated union"
    $ do
      FilterAll `shouldEncodeTo` "{\"tag\":\"FilterAll\"}"
      FilterSingle (BmSlug "abc123") `shouldEncodeTo` "{\"tag\":\"FilterSingle\",\"contents\":\"abc123\"}"

  describe "TagCloudMode (app.tagCloudMode)"
    $ it "encodes each variant's mode/value/expanded/lowerBound shape"
    $ do
      TagCloudModeTop True `shouldEncodeTo` "{\"mode\":\"top\",\"expanded\":true}"
      TagCloudModeTopLowerBound False 20 `shouldEncodeTo` "{\"mode\":\"lowerBound\",\"value\":20,\"expanded\":false}"
      TagCloudModeRelated True ["foo", "bar"] `shouldEncodeTo` "{\"mode\":\"related\",\"value\":\"foo bar\",\"expanded\":true}"
      TagCloudModeRelatedLowerBound False ["foo"] 5 `shouldEncodeTo` "{\"mode\":\"relatedLowerBound\",\"value\":\"foo\",\"lowerBound\":5,\"expanded\":false}"
      TagCloudModeNone `shouldEncodeTo` "{\"mode\":\"none\",\"value\":null,\"expanded\":false}"

  describe "TagSuggestionRequest / TagSuggestionResponse (api/tagSuggestions)" $ do
    it "decodes a frontend-submitted request"
      $ "{\"query\":\"fo\",\"currentTags\":[\"bar\"]}"
      `shouldDecodeTo` TagSuggestionRequest {_query = "fo", _currentTags = Just ["bar"]}

    it "encodes the suggestion list"
      $ TagSuggestionResponse {_suggestions = [Suggestion "foo" 3, Suggestion "food" 1]}
      `shouldEncodeTo` "{\"suggestions\":[{\"term\":\"foo\",\"count\":3},{\"term\":\"food\",\"count\":1}]}"

  describe "BulkEditForm (api/bm/bulkEdit request body)" $ do
    it "decodes a page selection"
      $ "{\"selection\":\"page\",\"bids\":[1,2,3],\"action\":\"star\",\"addTags\":\"foo\",\"removeTags\":\"bar\",\"selectionCount\":3}"
      `shouldDecodeTo` BulkEditForm
        { _beSelection = BulkSelectionPage [1, 2, 3],
          _beAction = Just BulkActionStar,
          _beAddTags = "foo",
          _beRemoveTags = "bar",
          _beSelectionCount = 3
        }

    it "decodes an all-matching-query selection"
      $ "{\"selection\":\"all\",\"filter\":{\"tag\":\"FilterAll\"},\"sharedp\":\"all\",\"tags\":[\"x\"],\"query\":\"q\",\"action\":null,\"addTags\":\"\",\"removeTags\":\"\",\"selectionCount\":10}"
      `shouldDecodeTo` BulkEditForm
        { _beSelection = BulkSelectionAll FilterAll SharedAll ["x"] (Just "q"),
          _beAction = Nothing,
          _beAddTags = "",
          _beRemoveTags = "",
          _beSelectionCount = 10
        }

  describe "NoteBulkEditForm (api/note/bulkEdit request body)" $ do
    it "decodes a page selection"
      $ "{\"selection\":\"page\",\"nids\":[5,6],\"action\":\"markdown\",\"selectionCount\":2}"
      `shouldDecodeTo` NoteBulkEditForm
        { _nbeSelection = NoteBulkSelectionPage [5, 6],
          _nbeAction = NoteBulkActionMarkdown,
          _nbeSelectionCount = 2
        }

    it "decodes an all-matching-query selection"
      $ "{\"selection\":\"all\",\"sharedp\":\"public\",\"query\":null,\"action\":\"delete\",\"selectionCount\":4}"
      `shouldDecodeTo` NoteBulkEditForm
        { _nbeSelection = NoteBulkSelectionAll SharedPublic Nothing,
          _nbeAction = NoteBulkActionDelete,
          _nbeSelectionCount = 4
        }

bookmarkFormFixture :: BookmarkForm
bookmarkFormFixture =
  BookmarkForm
    { _url = "https://example.com",
      _title = Just "Example",
      _description = Just (Textarea "An extended description"),
      _tags = Just "foo bar",
      _private = Just False,
      _toread = Just True,
      _bid = Just 1,
      _slug = Just (BmSlug "abc123"),
      _selected = Just True,
      _time = Just (UTCTimeStr t0),
      _archiveUrl = Just "https://archive.example.com/x",
      _archiveRequested = Just True
    }

noteFixture :: Entity Note
noteFixture =
  Entity
    (toSqlKey 42)
    Note
      { noteUserId = toSqlKey 7,
        noteSlug = NtSlug "abcdefghij",
        noteLength = 4,
        noteTitle = "Title",
        noteText = "Body text",
        noteIsMarkdown = True,
        noteShared = False,
        noteCreated = t0,
        noteUpdated = t1
      }

testUser :: User
testUser = User "u" (PasswordHash "h") Nothing False False True True False False True Nothing
