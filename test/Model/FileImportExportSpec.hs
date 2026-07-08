{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Tests deserialize bookmark/note data from the same raw formats used at rest

module Model.FileImportExportSpec (spec) where

import Data.Aeson qualified as A
import Model.Custom (hashPasswordBCryptWithPolicy)
import Model.FileBookmark
import Model.FileFirefox
import Model.FileNetscape
import Model.FileNote
import TestImport
import Types (DB)

-- ---------------------------------------------------------------------------
-- Pinboard JSON export format.
-- shared/toread are "yes"/"no" strings (not booleans).
-- description may be the boolean false when a bookmark has no title.
-- ---------------------------------------------------------------------------

-- Single public bookmark, two space-separated tags.
bmJson :: ByteString
bmJson =
  mconcat
    [ "[{\"href\":\"https://example.com\"",
      ",\"description\":\"Example\"",
      ",\"extended\":\"An extended description\"",
      ",\"time\":\"2024-01-01T00:00:00Z\"",
      ",\"shared\":\"yes\"",
      ",\"toread\":\"no\"",
      ",\"tags\":\"foo bar\"",
      "}]"
    ]

-- Private bookmark: shared=no, toread=yes, selected=true, archive_url, .secret tag.
bmPrivateJson :: ByteString
bmPrivateJson =
  mconcat
    [ "[{\"href\":\"https://private.com\"",
      ",\"description\":\"Private bookmark\"",
      ",\"extended\":\"\"",
      ",\"time\":\"2024-06-15T12:00:00Z\"",
      ",\"shared\":\"no\"",
      ",\"toread\":\"yes\"",
      ",\"selected\":true",
      ",\"archive_url\":\"https://archive.example.com\"",
      ",\"tags\":\".secret tag2\"",
      "}]"
    ]

-- Two bookmarks in one array.
bmMultiJson :: ByteString
bmMultiJson =
  mconcat
    [ "[{\"href\":\"https://a.com\"",
      ",\"description\":\"A\"",
      ",\"extended\":\"\"",
      ",\"time\":\"2024-01-01T00:00:00Z\"",
      ",\"shared\":\"yes\"",
      ",\"toread\":\"no\"",
      ",\"tags\":\"alpha\"",
      "}",
      ",{\"href\":\"https://b.com\"",
      ",\"description\":\"B\"",
      ",\"extended\":\"\"",
      ",\"time\":\"2024-01-02T00:00:00Z\"",
      ",\"shared\":\"yes\"",
      ",\"toread\":\"no\"",
      ",\"tags\":\"beta\"",
      "}]"
    ]

-- description is the boolean false (Pinboard's encoding for no title).
bmFalseDescJson :: ByteString
bmFalseDescJson =
  mconcat
    [ "[{\"href\":\"https://nodesc.com\"",
      ",\"description\":false",
      ",\"extended\":\"\"",
      ",\"time\":\"2024-01-01T00:00:00Z\"",
      ",\"shared\":\"yes\"",
      ",\"toread\":\"no\"",
      ",\"tags\":\"\"",
      "}]"
    ]

-- All optional fields absent: no selected, no archive_url.
bmMinimalJson :: ByteString
bmMinimalJson =
  mconcat
    [ "[{\"href\":\"https://minimal.com\"",
      ",\"description\":\"Minimal\"",
      ",\"extended\":\"\"",
      ",\"time\":\"2024-01-01T00:00:00Z\"",
      ",\"shared\":\"no\"",
      ",\"toread\":\"no\"",
      ",\"tags\":\"\"",
      "}]"
    ]

-- ---------------------------------------------------------------------------
-- Firefox JSON export format.
-- dateAdded and lastModified are in microseconds since epoch.
-- ---------------------------------------------------------------------------

-- Single typeCode=1 bookmark node.
firefoxBookmarkJson :: ByteString
firefoxBookmarkJson =
  mconcat
    [ "{\"typeCode\":1",
      ",\"type\":\"text/x-moz-place\"",
      ",\"dateAdded\":1704067200000000",
      ",\"lastModified\":1704067200000000",
      ",\"guid\":\"bm-guid-1\"",
      ",\"id\":2",
      ",\"index\":0",
      ",\"title\":\"Firefox BM\"",
      ",\"uri\":\"https://firefox.com\"",
      "}"
    ]

-- typeCode=2 folder with two children.
firefoxFolderJson :: ByteString
firefoxFolderJson =
  mconcat
    [ "{\"typeCode\":2",
      ",\"type\":\"text/x-moz-place-container\"",
      ",\"dateAdded\":0",
      ",\"lastModified\":0",
      ",\"guid\":\"folder-guid\"",
      ",\"id\":1",
      ",\"index\":0",
      ",\"title\":\"Bookmarks Menu\"",
      ",\"children\":[",
      "{\"typeCode\":1",
      ",\"type\":\"text/x-moz-place\"",
      ",\"dateAdded\":1704067200000000",
      ",\"lastModified\":1704067200000000",
      ",\"guid\":\"bm-guid-a\"",
      ",\"id\":2",
      ",\"index\":0",
      ",\"title\":\"Site A\"",
      ",\"uri\":\"https://site-a.com\"",
      "}",
      ",{\"typeCode\":1",
      ",\"type\":\"text/x-moz-place\"",
      ",\"dateAdded\":1704153600000000",
      ",\"lastModified\":1704153600000000",
      ",\"guid\":\"bm-guid-b\"",
      ",\"id\":3",
      ",\"index\":1",
      ",\"title\":\"Site B\"",
      ",\"uri\":\"https://site-b.com\"",
      "}",
      "]}"
    ]

-- typeCode=3 separator: no children, no uri.
firefoxSeparatorJson :: ByteString
firefoxSeparatorJson =
  mconcat
    [ "{\"typeCode\":3",
      ",\"type\":\"text/x-moz-place-separator\"",
      ",\"dateAdded\":0",
      ",\"lastModified\":0",
      ",\"guid\":\"sep-guid\"",
      ",\"id\":5",
      ",\"index\":0",
      ",\"title\":\"\"",
      "}"
    ]

-- Nested folders: Root > leaf1, Inner > leaf2.
firefoxNestedJson :: ByteString
firefoxNestedJson =
  mconcat
    [ "{\"typeCode\":2",
      ",\"type\":\"text/x-moz-place-container\"",
      ",\"dateAdded\":0,\"lastModified\":0",
      ",\"guid\":\"root-guid\",\"id\":1,\"index\":0",
      ",\"title\":\"Root\"",
      ",\"children\":[",
      "{\"typeCode\":1",
      ",\"type\":\"text/x-moz-place\"",
      ",\"dateAdded\":1704067200000000,\"lastModified\":1704067200000000",
      ",\"guid\":\"leaf1-guid\",\"id\":2,\"index\":0",
      ",\"title\":\"Leaf 1\",\"uri\":\"https://leaf1.com\"",
      "}",
      ",{\"typeCode\":2",
      ",\"type\":\"text/x-moz-place-container\"",
      ",\"dateAdded\":0,\"lastModified\":0",
      ",\"guid\":\"inner-guid\",\"id\":3,\"index\":1",
      ",\"title\":\"Inner Folder\"",
      ",\"children\":[",
      "{\"typeCode\":1",
      ",\"type\":\"text/x-moz-place\"",
      ",\"dateAdded\":1704067200000000,\"lastModified\":1704067200000000",
      ",\"guid\":\"leaf2-guid\",\"id\":4,\"index\":0",
      ",\"title\":\"Leaf 2\",\"uri\":\"https://leaf2.com\"",
      "}",
      "]}",
      "]}"
    ]

-- ---------------------------------------------------------------------------
-- Netscape HTML format (ADD_DATE in POSIX seconds, TAGS comma or space sep).
-- ---------------------------------------------------------------------------

netscapeSimpleHtml :: Text
netscapeSimpleHtml =
  unlines
    [ "<!DOCTYPE NETSCAPE-Bookmark-file-1>",
      "<META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=UTF-8\">",
      "<TITLE>Bookmarks</TITLE>",
      "<H1>Bookmarks</H1>",
      "<DL><p>",
      "    <DT><A HREF=\"https://ns.com\" ADD_DATE=\"1704067200\" TAGS=\"foo bar\">NS Title</A>",
      "    <DD>Some description",
      "</DL><p>"
    ]

-- Comma-separated TAGS (alternative convention).
netscapeCommaSepHtml :: Text
netscapeCommaSepHtml =
  unlines
    [ "<!DOCTYPE NETSCAPE-Bookmark-file-1>",
      "<DL><p>",
      "    <DT><A HREF=\"https://comma.com\" ADD_DATE=\"1704067200\" TAGS=\"foo,bar,baz\">Comma Tags</A>",
      "</DL><p>"
    ]

-- Multiple DT entries.
netscapeMultiHtml :: Text
netscapeMultiHtml =
  unlines
    [ "<!DOCTYPE NETSCAPE-Bookmark-file-1>",
      "<DL><p>",
      "    <DT><A HREF=\"https://first.com\" ADD_DATE=\"1704067200\" TAGS=\"tag1\">First</A>",
      "    <DT><A HREF=\"https://second.com\" ADD_DATE=\"1704153600\" TAGS=\"tag2\">Second</A>",
      "</DL><p>"
    ]

-- HTML entities in title and href.
netscapeEntitiesHtml :: Text
netscapeEntitiesHtml =
  unlines
    [ "<!DOCTYPE NETSCAPE-Bookmark-file-1>",
      "<DL><p>",
      "    <DT><A HREF=\"https://entity.com/path?a=1&amp;b=2\" ADD_DATE=\"1704067200\" TAGS=\"\">Title &amp; More</A>",
      "</DL><p>"
    ]

-- ---------------------------------------------------------------------------
-- FileNote JSON format.
-- times use "%F %T" ("2024-01-01 00:00:00"), not ISO 8601.
-- ---------------------------------------------------------------------------

noteJson :: ByteString
noteJson =
  mconcat
    [ "{\"id\":\"note-1\"",
      ",\"title\":\"My Title\"",
      ",\"text\":\"Body text\"",
      ",\"length\":9",
      ",\"created_at\":\"2024-01-01 00:00:00\"",
      ",\"updated_at\":\"2024-01-01 00:00:00\"",
      "}"
    ]

note2Json :: ByteString
note2Json =
  mconcat
    [ "{\"id\":\"note-2\"",
      ",\"title\":\"Second Note\"",
      ",\"text\":\"More text here\"",
      ",\"length\":14",
      ",\"created_at\":\"2024-06-15 12:00:00\"",
      ",\"updated_at\":\"2024-06-15 12:00:00\"",
      "}"
    ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

createTestUser :: DB (Key User)
createTestUser = do
  pwHash <- liftIO $ hashPasswordBCryptWithPolicy bcryptTestPolicy "pass"
  insert $ User "testuser" pwHash Nothing False False True False False True Nothing

-- Decode ByteString or abort the test with an informative message.
decodeOrFail :: (A.FromJSON a) => ByteString -> IO a
decodeOrFail bs = case A.eitherDecode' (fromStrict bs) of
  Left e -> throwString ("JSON decode failed: " <> e)
  Right v -> pure v

-- Get the first element of a list or abort the test.
firstOf :: [a] -> IO a
firstOf (x : _) = pure x
firstOf [] = throwString "expected non-empty list"

-- Get the second element of a list or abort the test.
secondOf :: [a] -> IO a
secondOf (_ : x : _) = pure x
secondOf _ = throwString "expected list with at least two elements"

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  -- Pure parse tests: no DB, no app fixture.
  describe "FileBookmark FromJSON" $ do
    it "parses a well-formed Pinboard bookmark" $ do
      fbs <- decodeOrFail bmJson :: IO [FileBookmark]
      length fbs `shouldBe` 1
      fb <- firstOf fbs
      fileBookmarkHref fb `shouldBe` "https://example.com"
      fileBookmarkDescription fb `shouldBe` "Example"
      fileBookmarkExtended fb `shouldBe` "An extended description"
      fileBookmarkShared fb `shouldBe` True
      fileBookmarkToRead fb `shouldBe` False
      fileBookmarkTags fb `shouldBe` "foo bar"

    it "parses description:false as empty string (Pinboard quirk)" $ do
      fbs <- decodeOrFail bmFalseDescJson :: IO [FileBookmark]
      fb <- firstOf fbs
      fileBookmarkDescription fb `shouldBe` ""

    it "parses missing optional fields as Nothing" $ do
      fbs <- decodeOrFail bmMinimalJson :: IO [FileBookmark]
      fb <- firstOf fbs
      fileBookmarkSelected fb `shouldBe` Nothing
      fileBookmarkArchiveHref fb `shouldBe` Nothing

    it "parses shared=no as False and toread=yes as True" $ do
      fbs <- decodeOrFail bmPrivateJson :: IO [FileBookmark]
      fb <- firstOf fbs
      fileBookmarkShared fb `shouldBe` False
      fileBookmarkToRead fb `shouldBe` True

    it "parses selected:true as Just True" $ do
      fbs <- decodeOrFail bmPrivateJson :: IO [FileBookmark]
      fb <- firstOf fbs
      fileBookmarkSelected fb `shouldBe` Just True

    it "parses archive_url" $ do
      fbs <- decodeOrFail bmPrivateJson :: IO [FileBookmark]
      fb <- firstOf fbs
      fileBookmarkArchiveHref fb `shouldBe` Just "https://archive.example.com"

    it "parses a two-bookmark array" $ do
      fbs <- decodeOrFail bmMultiJson :: IO [FileBookmark]
      length fbs `shouldBe` 2
      fb0 <- firstOf fbs
      fb1 <- secondOf fbs
      fileBookmarkHref fb0 `shouldBe` "https://a.com"
      fileBookmarkHref fb1 `shouldBe` "https://b.com"

    it "round-trips through encode then decode" $ do
      fbs <- decodeOrFail bmJson :: IO [FileBookmark]
      A.decode (A.encode fbs) `shouldBe` Just fbs

  describe "FirefoxBookmarkNode FromJSON" $ do
    it "parses a typeCode=1 bookmark node" $ do
      node <- decodeOrFail firefoxBookmarkJson :: IO FirefoxBookmarkNode
      firefoxBookmarkTypeCode node `shouldBe` 1
      firefoxBookmarkUri node `shouldBe` Just "https://firefox.com"
      firefoxBookmarkTitle node `shouldBe` "Firefox BM"

    it "parses a typeCode=2 folder with two children" $ do
      node <- decodeOrFail firefoxFolderJson :: IO FirefoxBookmarkNode
      firefoxBookmarkTypeCode node `shouldBe` 2
      length (fromMaybe [] (firefoxBookmarkChildren node)) `shouldBe` 2

    it "parses a typeCode=3 separator (no children, no uri)" $ do
      node <- decodeOrFail firefoxSeparatorJson :: IO FirefoxBookmarkNode
      firefoxBookmarkTypeCode node `shouldBe` 3
      firefoxBookmarkChildren node `shouldBe` Nothing
      firefoxBookmarkUri node `shouldBe` Nothing

  describe "NetscapeBookmark parse" $ do
    it "parses a single bookmark from HTML" $ do
      case parseNetscapeBookmarks netscapeSimpleHtml of
        Left e -> expectationFailure e
        Right bms -> do
          length bms `shouldBe` 1
          bm <- firstOf bms
          nsbHref bm `shouldBe` "https://ns.com"
          nsbTitle bm `shouldBe` "NS Title"
          nsbDescription bm `shouldBe` "Some description"
          nsbTags bm `shouldBe` "foo bar"

    it "parses comma-separated TAGS attribute" $ do
      case parseNetscapeBookmarks netscapeCommaSepHtml of
        Left e -> expectationFailure e
        Right bms -> do
          bm <- firstOf bms
          nsbTags bm `shouldBe` "foo,bar,baz"

    it "parses multiple DT entries" $ do
      case parseNetscapeBookmarks netscapeMultiHtml of
        Left e -> expectationFailure e
        Right bms -> do
          length bms `shouldBe` 2
          bm0 <- firstOf bms
          bm1 <- secondOf bms
          nsbHref bm0 `shouldBe` "https://first.com"
          nsbHref bm1 `shouldBe` "https://second.com"

    it "decodes HTML entities in title and href" $ do
      case parseNetscapeBookmarks netscapeEntitiesHtml of
        Left e -> expectationFailure e
        Right bms -> do
          bm <- firstOf bms
          nsbTitle bm `shouldBe` "Title & More"
          nsbHref bm `shouldBe` "https://entity.com/path?a=1&b=2"

  describe "FileNote FromJSON" $ do
    it "parses a well-formed note" $ do
      fn <- decodeOrFail noteJson :: IO FileNote
      fileNoteId fn `shouldBe` "note-1"
      fileNoteTitle fn `shouldBe` "My Title"
      fileNoteText fn `shouldBe` "Body text"
      fileNoteLength fn `shouldBe` 9

    it "round-trips through encode then decode" $ do
      fn <- decodeOrFail noteJson :: IO FileNote
      A.decode (A.encode fn) `shouldBe` Just fn

  withApp $ do
    -- -----------------------------------------------------------------
    describe "insertFileBookmarks (from JSON)" $ do
      it "inserts a parsed bookmark and retrieves via getFileBookmarks" $ do
        uid <- runDB createTestUser
        fbs <- liftIO $ decodeOrFail bmJson
        n <- runDB $ insertFileBookmarks uid fbs
        liftIO $ n `shouldBe` 1
        result <- runDB $ getFileBookmarks uid
        liftIO $ length result `shouldBe` 1
        liftIO $ fmap fileBookmarkHref (headMay result) `shouldBe` Just "https://example.com"
        liftIO $ fmap fileBookmarkTags (headMay result) `shouldBe` Just "foo bar"

      it "inserts multiple parsed bookmarks" $ do
        uid <- runDB createTestUser
        fbs <- liftIO $ decodeOrFail bmMultiJson
        n <- runDB $ insertFileBookmarks uid fbs
        liftIO $ n `shouldBe` 2
        result <- runDB $ getFileBookmarks uid
        liftIO $ length result `shouldBe` 2

      it "skips duplicate hrefs on re-insert" $ do
        uid <- runDB createTestUser
        fbs <- liftIO $ decodeOrFail bmJson
        n1 <- runDB $ insertFileBookmarks uid fbs
        n2 <- runDB $ insertFileBookmarks uid fbs
        liftIO $ n1 `shouldBe` 1
        liftIO $ n2 `shouldBe` 1
        result <- runDB $ getFileBookmarks uid
        liftIO $ length result `shouldBe` 1

    -- -----------------------------------------------------------------
    describe "FileBookmark round-trip (JSON -> insert -> getFileBookmarks)" $ do
      it "preserves href, description, extended, shared, toRead" $ do
        uid <- runDB createTestUser
        fbs0 <- liftIO $ decodeOrFail bmJson
        fb <- liftIO $ firstOf fbs0
        _ <- runDB $ insertFileBookmarks uid [fb]
        result <- runDB $ getFileBookmarks uid
        case headMay result of
          Nothing -> liftIO $ expectationFailure "expected non-empty result"
          Just got -> liftIO $ do
            fileBookmarkHref got `shouldBe` fileBookmarkHref fb
            fileBookmarkDescription got `shouldBe` fileBookmarkDescription fb
            fileBookmarkExtended got `shouldBe` fileBookmarkExtended fb
            fileBookmarkShared got `shouldBe` fileBookmarkShared fb
            fileBookmarkToRead got `shouldBe` fileBookmarkToRead fb

      it "normalises absent selected to Just False" $ do
        uid <- runDB createTestUser
        fbs1 <- liftIO $ decodeOrFail bmJson
        fb1 <- liftIO $ firstOf fbs1
        liftIO $ fileBookmarkSelected fb1 `shouldBe` Nothing
        _ <- runDB $ insertFileBookmarks uid [fb1]
        result <- runDB $ getFileBookmarks uid
        liftIO $ fmap fileBookmarkSelected (headMay result) `shouldBe` Just (Just False)

      it "preserves selected=True parsed from JSON" $ do
        uid <- runDB createTestUser
        fbs2 <- liftIO $ decodeOrFail bmPrivateJson
        fb2 <- liftIO $ firstOf fbs2
        liftIO $ fileBookmarkSelected fb2 `shouldBe` Just True
        _ <- runDB $ insertFileBookmarks uid [fb2]
        result <- runDB $ getFileBookmarks uid
        liftIO $ fmap fileBookmarkSelected (headMay result) `shouldBe` Just (Just True)

      it "preserves archiveHref parsed from JSON" $ do
        uid <- runDB createTestUser
        fbs3 <- liftIO $ decodeOrFail bmPrivateJson
        fb3 <- liftIO $ firstOf fbs3
        _ <- runDB $ insertFileBookmarks uid [fb3]
        result <- runDB $ getFileBookmarks uid
        liftIO $ fmap fileBookmarkArchiveHref (headMay result) `shouldBe` Just (Just "https://archive.example.com")

      it "description:false parses as empty and survives round-trip" $ do
        uid <- runDB createTestUser
        fbs4 <- liftIO $ decodeOrFail bmFalseDescJson
        fb4 <- liftIO $ firstOf fbs4
        liftIO $ fileBookmarkDescription fb4 `shouldBe` ""
        _ <- runDB $ insertFileBookmarks uid [fb4]
        result <- runDB $ getFileBookmarks uid
        liftIO $ fmap fileBookmarkDescription (headMay result) `shouldBe` Just ""

    -- -----------------------------------------------------------------
    describe "insertFirefoxBookmarks (from JSON)" $ do
      it "inserts a typeCode=1 bookmark" $ do
        uid <- runDB createTestUser
        node <- liftIO $ decodeOrFail firefoxBookmarkJson
        n <- runDB $ insertFirefoxBookmarks uid node
        liftIO $ n `shouldBe` 1
        result <- runDB $ getFileBookmarks uid
        liftIO $ fmap fileBookmarkHref (headMay result) `shouldBe` Just "https://firefox.com"
        liftIO $ fmap fileBookmarkDescription (headMay result) `shouldBe` Just "Firefox BM"

      it "inserts all children of a typeCode=2 folder" $ do
        uid <- runDB createTestUser
        node <- liftIO $ decodeOrFail firefoxFolderJson
        n <- runDB $ insertFirefoxBookmarks uid node
        liftIO $ n `shouldBe` 2
        result <- runDB $ getFileBookmarks uid
        liftIO $ length result `shouldBe` 2

      it "inserts nothing for a typeCode=3 separator" $ do
        uid <- runDB createTestUser
        node <- liftIO $ decodeOrFail firefoxSeparatorJson
        n <- runDB $ insertFirefoxBookmarks uid node
        liftIO $ n `shouldBe` 0

      it "flattens bookmarks from nested folders" $ do
        uid <- runDB createTestUser
        node <- liftIO $ decodeOrFail firefoxNestedJson
        n <- runDB $ insertFirefoxBookmarks uid node
        liftIO $ n `shouldBe` 2
        result <- runDB $ getFileBookmarks uid
        liftIO $ length result `shouldBe` 2

    -- -----------------------------------------------------------------
    describe "insertNetscapeBookmarks (from HTML)" $ do
      it "inserts a bookmark parsed from HTML" $ do
        uid <- runDB createTestUser
        case parseNetscapeBookmarks netscapeSimpleHtml of
          Left e -> liftIO $ expectationFailure e
          Right nbms -> do
            n <- runDB $ insertNetscapeBookmarks uid nbms
            liftIO $ n `shouldBe` 1

      it "comma-separated TAGS are split on insert" $ do
        uid <- runDB createTestUser
        case parseNetscapeBookmarks netscapeCommaSepHtml of
          Left e -> liftIO $ expectationFailure e
          Right nbms -> do
            _ <- runDB $ insertNetscapeBookmarks uid nbms
            result <- runDB $ getNetscapeBookmarks uid
            liftIO $ fmap nsbTags (headMay result) `shouldBe` Just "foo bar baz"

      it "inserts multiple bookmarks from HTML" $ do
        uid <- runDB createTestUser
        case parseNetscapeBookmarks netscapeMultiHtml of
          Left e -> liftIO $ expectationFailure e
          Right nbms -> do
            n <- runDB $ insertNetscapeBookmarks uid nbms
            liftIO $ n `shouldBe` 2

    -- -----------------------------------------------------------------
    describe "NetscapeBookmark round-trip (HTML -> insert -> getNetscapeBookmarks -> render -> reparse)" $ do
      it "preserves href, title, description through insert/export" $ do
        uid <- runDB createTestUser
        case parseNetscapeBookmarks netscapeSimpleHtml of
          Left e -> liftIO $ expectationFailure e
          Right nbms -> do
            _ <- runDB $ insertNetscapeBookmarks uid nbms
            exported <- runDB $ getNetscapeBookmarks uid
            case headMay exported of
              Nothing -> liftIO $ expectationFailure "expected non-empty export"
              Just bm -> liftIO $ do
                nsbHref bm `shouldBe` "https://ns.com"
                nsbTitle bm `shouldBe` "NS Title"
                nsbDescription bm `shouldBe` "Some description"

      it "space-separated tags survive insert/export unchanged" $ do
        uid <- runDB createTestUser
        case parseNetscapeBookmarks netscapeSimpleHtml of
          Left e -> liftIO $ expectationFailure e
          Right nbms -> do
            _ <- runDB $ insertNetscapeBookmarks uid nbms
            exported <- runDB $ getNetscapeBookmarks uid
            liftIO $ fmap nsbTags (headMay exported) `shouldBe` Just "foo bar"

      it "rendered HTML re-parses to same href and title" $ do
        uid <- runDB createTestUser
        case parseNetscapeBookmarks netscapeSimpleHtml of
          Left e -> liftIO $ expectationFailure e
          Right nbms -> do
            _ <- runDB $ insertNetscapeBookmarks uid nbms
            exported <- runDB $ getNetscapeBookmarks uid
            let rendered = renderNetscapeBookmarks exported
            case parseNetscapeBookmarks rendered of
              Left e -> liftIO $ expectationFailure ("re-parse failed: " <> e)
              Right reimported -> liftIO $ do
                fmap nsbHref (headMay reimported) `shouldBe` Just "https://ns.com"
                fmap nsbTitle (headMay reimported) `shouldBe` Just "NS Title"

    -- -----------------------------------------------------------------
    describe "insertFileNotes (from JSON)" $ do
      it "inserts a note deserialized from JSON" $ do
        uid <- runDB createTestUser
        fn <- liftIO $ decodeOrFail noteJson
        n <- runDB $ insertFileNotes uid [fn]
        liftIO $ n `shouldBe` 1

      it "inserts multiple notes" $ do
        uid <- runDB createTestUser
        fn1 <- liftIO $ decodeOrFail noteJson
        fn2 <- liftIO $ decodeOrFail note2Json
        n <- runDB $ insertFileNotes uid [fn1, fn2]
        liftIO $ n `shouldBe` 2

      it "skips duplicate notes on re-insert" $ do
        uid <- runDB createTestUser
        fn <- liftIO $ decodeOrFail noteJson
        n1 <- runDB $ insertFileNotes uid [fn]
        n2 <- runDB $ insertFileNotes uid [fn]
        liftIO $ n1 `shouldBe` 1
        liftIO $ n2 `shouldBe` 1

    -- -----------------------------------------------------------------
    describe "getFileNotes (notes export)" $ do
      it "exports inserted notes ordered by created, preserving title/text/length" $ do
        uid <- runDB createTestUser
        fn1 <- liftIO $ decodeOrFail noteJson
        fn2 <- liftIO $ decodeOrFail note2Json
        _ <- runDB $ insertFileNotes uid [fn1, fn2]
        result <- runDB $ getFileNotes uid
        liftIO $ map fileNoteTitle result `shouldBe` ["My Title", "Second Note"]
        liftIO $ fmap fileNoteText (headMay result) `shouldBe` Just "Body text"
        liftIO $ fmap fileNoteLength (headMay result) `shouldBe` Just 9

      it "round-trips notes through insert then export" $ do
        uid <- runDB createTestUser
        fn <- liftIO $ decodeOrFail noteJson
        _ <- runDB $ insertFileNotes uid [fn]
        result <- runDB $ getFileNotes uid
        case headMay result of
          Nothing -> liftIO $ expectationFailure "expected non-empty export"
          Just got -> liftIO $ do
            fileNoteTitle got `shouldBe` fileNoteTitle fn
            fileNoteText got `shouldBe` fileNoteText fn
            fileNoteCreatedAt got `shouldBe` fileNoteCreatedAt fn
            fileNoteUpdatedAt got `shouldBe` fileNoteUpdatedAt fn
