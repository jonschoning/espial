{-# LANGUAGE DeriveGeneric #-}

module Model.FileNetscape where

import ClassyPrelude.Yesod hiding (Value, exists, groupBy, on, (<=.), (==.), (>=.), (||.))
import Data.Char (isSpace)
import Data.List (elemIndex, findIndex)
import Data.Text (breakOn, splitOn, strip)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Time.Clock.POSIX as TI (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import qualified HTMLEntities.Decoder as HED
import qualified HTMLEntities.Text as HE
import Model
import Model.Custom
import Types

-- * Netscape Bookmarks

data NetscapeBookmark = NetscapeBookmark
  { nsbHref :: !Text,
    nsbTitle :: !Text,
    nsbDescription :: !Text,
    nsbTags :: !Text,
    nsbTime :: !UTCTime
  }
  deriving (Eq, Typeable, Ord, Show)

parseNetscapeBookmarks :: Text -> Either String [NetscapeBookmark]
parseNetscapeBookmarks content = do
  let links = extractLinks
  if null links
    then Left "No bookmarks found"
    else Right links
  where
    -- Extract bookmark links from NETSCAPE-Bookmark-file HTML.
    -- Split on "<DT>" so each segment corresponds to one entry.  A <DD>
    -- following the </A> within that same segment is the description for that
    -- entry (matching the Netscape model: DD belongs to the last seen DT).
    extractLinks :: [NetscapeBookmark]
    extractLinks =
      let dtSegments = drop 1 $ splitOnDT content
          anchorSegments = filter (\s -> "<A " `isInfixOf` s && "</A>" `isInfixOf` s) dtSegments
       in catMaybes (_parseNetscapeLink <$> anchorSegments)

    -- Split content into segments at each "<DT>", discarding any text before
    -- the first marker.
    splitOnDT :: Text -> [Text]
    splitOnDT txt =
      case breakOn "<DT>" txt of
        (_, "") -> [txt]
        (before, rest) -> before : splitOnDT (drop 4 rest) -- drop "<DT>" (4 chars)

-- Parse a single <A HREF="..." ...>Title</A> line
_parseNetscapeLink :: Text -> Maybe NetscapeBookmark
_parseNetscapeLink line = do
  guard (("<A " `isInfixOf` line) && ("</A>" `isInfixOf` line))
  href <- extractAttr "HREF"
  let lineStr = unpack line
      aIdx = findIndex (isPrefixOf "<A ") (tails lineStr)
  case aIdx of
    Nothing -> Nothing
    Just idx ->
      let aTag = drop idx lineStr
          gtIdx = elemIndex '>' aTag
       in case gtIdx of
            Nothing -> Nothing
            Just gtOffset ->
              let afterGt = drop (gtOffset + 1) aTag
                  ltIdx = elemIndex '<' afterGt
                  titleStr = case ltIdx of
                    Nothing -> afterGt
                    Just ltidx -> take ltidx afterGt
                  title = strip (decodeHtml (pack titleStr))
               in do
                    guard (not (null href))
                    let tags = fromMaybe "" (decodeHtml <$> extractAttr "TAGS")
                        timeStr = fromMaybe "" (extractAttr "ADD_DATE")
                        timeInt = readMay timeStr :: Maybe Int
                        utcTime = maybe (UTCTime (toEnum 0) 0) (TI.posixSecondsToUTCTime . fromIntegral) timeInt
                    pure
                      $ NetscapeBookmark
                        { nsbHref = href,
                          nsbTitle = title,
                          nsbDescription = extractDD,
                          nsbTags = tags,
                          nsbTime = utcTime
                        }
  where
    -- Extract description text from a <DD> tag that follows </A> in the chunk.
    extractDD :: Text
    extractDD =
      let afterClose = snd (breakOn "</A>" line)
          afterDD = snd (breakOn "<DD>" afterClose)
       in if null afterDD
            then ""
            else
              let inner = drop 4 afterDD -- drop "<DD>"
                  trimmed = fst (breakOn "<" inner)
                  trimmedStr = dropWhile isSpace . reverse . dropWhile isSpace . reverse . unpack $ trimmed
               in decodeHtml (pack trimmedStr)

    -- Extract an HTML attribute value (e.g., HREF="value" -> value)
    extractAttr :: Text -> Maybe Text
    extractAttr attr =
      let attrPattern = unpack attr <> "=\""
          lineStr = unpack line
       in case dropWhile (\s -> not (attrPattern `isPrefixOf` s)) (tails lineStr) of
            [] -> Nothing
            (match : _) ->
              let afterPattern = drop (length attrPattern) match
               in case elemIndex '"' afterPattern of
                    Nothing -> Nothing
                    Just endIdx ->
                      let extracted = take endIdx afterPattern
                       in if null extracted then Nothing else Just (decodeHtml (pack extracted))

    decodeHtml :: Text -> Text
    decodeHtml = TL.toStrict . TLB.toLazyText . HED.htmlEncodedText

netscapeBookmarkToBookmark :: UserId -> NetscapeBookmark -> IO Bookmark
netscapeBookmarkToBookmark user NetscapeBookmark {..} = do
  slug <- mkBmSlug
  pure
    $ Bookmark
      { bookmarkUserId = user,
        bookmarkSlug = slug,
        bookmarkHref = nsbHref,
        bookmarkDescription = nsbTitle,
        bookmarkExtended = nsbDescription,
        bookmarkTime = nsbTime,
        bookmarkShared = True,
        bookmarkToRead = False,
        bookmarkSelected = False,
        bookmarkArchiveHref = Nothing
      }

bookmarkToNetscapeBookmark :: Bookmark -> Text -> NetscapeBookmark
bookmarkToNetscapeBookmark Bookmark {..} tags =
  NetscapeBookmark
    { nsbHref = bookmarkHref,
      nsbTitle = bookmarkDescription,
      nsbDescription = bookmarkExtended,
      nsbTags = tags,
      nsbTime = bookmarkTime
    }

renderNetscapeBookmarks :: [NetscapeBookmark] -> Text
renderNetscapeBookmarks bmarks =
  unlines
    $ [ "<!DOCTYPE NETSCAPE-Bookmark-file-1>",
        "<META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=UTF-8\">",
        "<TITLE>Bookmarks</TITLE>",
        "<H1>Bookmarks</H1>",
        "<DL><p>"
      ]
    <> concatMap renderBookmark bmarks
    <> ["</DL><p>"]
  where
    renderBookmark :: NetscapeBookmark -> [Text]
    renderBookmark NetscapeBookmark {..} =
      [ "    <DT><A HREF=\""
          <> escapeHtml nsbHref
          <> "\" ADD_DATE=\""
          <> tshow (floor (TI.utcTimeToPOSIXSeconds nsbTime) :: Int)
          <> "\" TAGS=\""
          <> escapeHtml nsbTags
          <> "\">"
          <> escapeHtml nsbTitle
          <> "</A>"
      ]
        <> ["    <DD>" <> escapeHtml nsbDescription | not (null (strip nsbDescription))]
    escapeHtml :: Text -> Text
    escapeHtml = HE.text

insertNetscapeBookmarks :: Key User -> FilePath -> DB (Either String Int)
insertNetscapeBookmarks userId bookmarkFile = do
  meither <- liftIO $ tryAny (readFile bookmarkFile)
  case meither of
    Left _ -> pure $ Left "Could not read file"
    Right (content :: ByteString) ->
      let contentText = decodeUtf8 content
       in case parseNetscapeBookmarks contentText of
            Left e -> pure $ Left e
            Right nbmarks -> do
              bmarks <- liftIO $ mapM (netscapeBookmarkToBookmark userId) nbmarks
              mbids <- mapM insertUnique bmarks
              mapM_ (void . insertUnique)
                $ concatMap (uncurry (mkBookmarkTags userId))
                $ catMaybes
                $ zipWith
                  (\mbid nbm -> (,parseNetscapeTags $ nsbTags nbm) <$> mbid)
                  mbids
                  nbmarks
              pure $ Right (length bmarks)
              where
                parseNetscapeTags :: Text -> [Text]
                parseNetscapeTags tagsText =
                  filter (not . null)
                    . map strip
                    $ if ',' `elem` unpack tagsText
                      then splitOn "," tagsText
                      else words tagsText

exportNetscapeBookmarks :: Key User -> FilePath -> DB ()
exportNetscapeBookmarks user fpath = do
  marks <- allUserBookmarks user
  let bmarks = fmap (\(bm, t) -> bookmarkToNetscapeBookmark (entityVal bm) t) marks
  liftIO $ writeFileUtf8 fpath (renderNetscapeBookmarks bmarks)
