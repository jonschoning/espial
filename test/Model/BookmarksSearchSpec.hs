{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Tests for the bookmark search query syntax documented in docs-search.hamlet.
--
-- Field map (DB name → UI name → search prefix):
--   BookmarkHref        url     url:/u:
--   BookmarkDescription title   title:/ti:
--   BookmarkExtended    notes   description:/d:
--   BookmarkTag         tags    tags:/t:
--   BookmarkTime        time    after:/a:  before:/b:

module Model.BookmarksSearchSpec (spec) where

import Model.Custom (hashPassword, mkBmSlug)
import TestImport
import Types (DB)

-- ─── Fixtures ──────────────────────────────────────────────────────────────

t2019 :: UTCTime
t2019 = UTCTime (fromGregorian 2019 1 1) 0

createTestUser :: DB (Key User)
createTestUser = do
  pwHash <- liftIO $ hashPassword "pass"
  insert $ User "testuser" pwHash Nothing False False True False False Nothing

-- href=url, description=title, extended=notes
createBm :: Key User -> Text -> Text -> Text -> UTCTime -> DB (Key Bookmark)
createBm uid href title notes time = do
  slug <- liftIO mkBmSlug
  insert $ Bookmark uid slug href title notes time False False False Nothing

tagBm :: Key User -> Key Bookmark -> Text -> Int -> DB ()
tagBm uid bid tag seq' = insert_ $ BookmarkTag uid tag bid seq'

search :: Key User -> Text -> DB [Key Bookmark]
search uid q = do
  (_, rows, _, _) <- bookmarksTagsQuery uid True SharedAll FilterAll [] (Just q) Nothing 100 1
  return $ map (entityKey . fst) rows

-- ─── Spec ──────────────────────────────────────────────────────────────────

spec :: Spec
spec = withApp $ do

  -- ─── Default (all-field) search ──────────────────────────────────────────

  describe "default (all-field) search" $ do
    it "matches a term found in the URL" $ do
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://youtube.com/watch" "" "" t2019
        return (uid, bid)
      bids <- runDB $ search uid "youtube"
      liftIO $ bids `shouldContain` [bid]

    it "matches a term found in the title (description field)" $ do
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://a.com" "Haskell tutorial" "" t2019
        return (uid, bid)
      bids <- runDB $ search uid "Haskell"
      liftIO $ bids `shouldContain` [bid]

    it "matches a term found in the notes (extended field)" $ do
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://a.com" "" "great introduction to monads" t2019
        return (uid, bid)
      bids <- runDB $ search uid "monads"
      liftIO $ bids `shouldContain` [bid]

    it "matches a term found in a tag" $ do
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://a.com" "" "" t2019
        tagBm uid bid "programming" 1
        return (uid, bid)
      bids <- runDB $ search uid "programming"
      liftIO $ bids `shouldContain` [bid]

    it "does not return a bookmark when the term matches nothing" $ do
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://a.com" "title" "notes" t2019
        return (uid, bid)
      bids <- runDB $ search uid "zzznomatch"
      liftIO $ bids `shouldNotContain` [bid]

  -- ─── url: / u: ───────────────────────────────────────────────────────────

  describe "url: field prefix" $ do
    it "url: matches a bookmark whose href contains the term" $ do
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://youtube.com/watch" "" "" t2019
        return (uid, bid)
      bids <- runDB $ search uid "url:youtube"
      liftIO $ bids `shouldContain` [bid]

    it "url: does not match a bookmark where the term is only in the title" $ do
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://a.com" "youtube tips" "" t2019
        return (uid, bid)
      bids <- runDB $ search uid "url:youtube"
      liftIO $ bids `shouldNotContain` [bid]

    it "u: is an alias for url:" $ do
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://youtube.com/watch" "" "" t2019
        return (uid, bid)
      bids <- runDB $ search uid "u:youtube"
      liftIO $ bids `shouldContain` [bid]

  -- ─── title: / ti: ────────────────────────────────────────────────────────

  describe "title: field prefix" $ do
    it "title: matches a bookmark whose title contains the term" $ do
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://a.com" "Haskell tutorial" "" t2019
        return (uid, bid)
      bids <- runDB $ search uid "title:Haskell"
      liftIO $ bids `shouldContain` [bid]

    it "title: does not match a bookmark where the term is only in the URL" $ do
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://haskell.org" "" "" t2019
        return (uid, bid)
      bids <- runDB $ search uid "title:haskell"
      liftIO $ bids `shouldNotContain` [bid]

    it "ti: is an alias for title:" $ do
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://a.com" "Haskell tutorial" "" t2019
        return (uid, bid)
      bids <- runDB $ search uid "ti:Haskell"
      liftIO $ bids `shouldContain` [bid]

  -- ─── description: / d: ───────────────────────────────────────────────────

  describe "description: field prefix" $ do
    it "description: matches a bookmark whose notes contain the term" $ do
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://a.com" "" "great intro to monads" t2019
        return (uid, bid)
      bids <- runDB $ search uid "description:monads"
      liftIO $ bids `shouldContain` [bid]

    it "description: does not match a bookmark where the term is only in the title" $ do
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://a.com" "monads explained" "" t2019
        return (uid, bid)
      bids <- runDB $ search uid "description:monads"
      liftIO $ bids `shouldNotContain` [bid]

    it "d: is an alias for description:" $ do
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://a.com" "" "great intro to monads" t2019
        return (uid, bid)
      bids <- runDB $ search uid "d:monads"
      liftIO $ bids `shouldContain` [bid]

  -- ─── tags: / t: ──────────────────────────────────────────────────────────

  describe "tags: field prefix" $ do
    it "tags: matches a bookmark with the given tag" $ do
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://a.com" "" "" t2019
        tagBm uid bid "haskell" 1
        return (uid, bid)
      bids <- runDB $ search uid "tags:haskell"
      liftIO $ bids `shouldContain` [bid]

    it "tags: does not match a bookmark where the term is only in the URL" $ do
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://haskell.org" "" "" t2019
        return (uid, bid)
      bids <- runDB $ search uid "tags:haskell"
      liftIO $ bids `shouldNotContain` [bid]

    it "t: is an alias for tags:" $ do
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://a.com" "" "" t2019
        tagBm uid bid "haskell" 1
        return (uid, bid)
      bids <- runDB $ search uid "t:haskell"
      liftIO $ bids `shouldContain` [bid]

  -- ─── after: / a: date filter ─────────────────────────────────────────────

  describe "after: date filter" $ do
    it "after:YYYY-MM-DD includes a bookmark on exactly that date" $ do
      let tExact = UTCTime (fromGregorian 2019 6 15) 0
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://a.com" "" "" tExact
        return (uid, bid)
      bids <- runDB $ search uid "after:2019-06-15"
      liftIO $ bids `shouldContain` [bid]

    it "after:YYYY-MM-DD excludes a bookmark one day before" $ do
      let tBefore = UTCTime (fromGregorian 2019 6 14) 0
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://a.com" "" "" tBefore
        return (uid, bid)
      bids <- runDB $ search uid "after:2019-06-15"
      liftIO $ bids `shouldNotContain` [bid]

    it "after:MM/DD/YYYY accepts the slash date format" $ do
      let tExact = UTCTime (fromGregorian 2019 6 15) 0
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://a.com" "" "" tExact
        return (uid, bid)
      bids <- runDB $ search uid "after:6/15/2019"
      liftIO $ bids `shouldContain` [bid]

    it "a: is an alias for after:" $ do
      let tExact = UTCTime (fromGregorian 2019 6 15) 0
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://a.com" "" "" tExact
        return (uid, bid)
      bids <- runDB $ search uid "a:2019-06-15"
      liftIO $ bids `shouldContain` [bid]

  -- ─── before: / b: date filter ────────────────────────────────────────────

  describe "before: date filter" $ do
    it "before:YYYY-MM-DD includes a bookmark on exactly that date" $ do
      let tExact = UTCTime (fromGregorian 2019 6 15) 0
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://a.com" "" "" tExact
        return (uid, bid)
      bids <- runDB $ search uid "before:2019-06-15"
      liftIO $ bids `shouldContain` [bid]

    it "before:YYYY-MM-DD excludes a bookmark one day after" $ do
      let tAfter = UTCTime (fromGregorian 2019 6 16) 0
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://a.com" "" "" tAfter
        return (uid, bid)
      bids <- runDB $ search uid "before:2019-06-15"
      liftIO $ bids `shouldNotContain` [bid]

    it "before:MM/DD/YYYY accepts the slash date format" $ do
      let tExact = UTCTime (fromGregorian 2019 6 15) 0
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://a.com" "" "" tExact
        return (uid, bid)
      bids <- runDB $ search uid "before:6/15/2019"
      liftIO $ bids `shouldContain` [bid]

    it "b: is an alias for before:" $ do
      let tExact = UTCTime (fromGregorian 2019 6 15) 0
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://a.com" "" "" tExact
        return (uid, bid)
      bids <- runDB $ search uid "b:2019-06-15"
      liftIO $ bids `shouldContain` [bid]

  -- ─── AND (space separator) ────────────────────────────────────────────────

  describe "AND combinator (space-separated terms)" $ do
    it "returns a bookmark matching both terms" $ do
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://youtube.com/haskell" "Haskell on YouTube" "" t2019
        return (uid, bid)
      bids <- runDB $ search uid "youtube haskell"
      liftIO $ bids `shouldContain` [bid]

    it "does not return a bookmark missing the second term" $ do
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://youtube.com/watch" "" "" t2019
        return (uid, bid)
      bids <- runDB $ search uid "youtube haskell"
      liftIO $ bids `shouldNotContain` [bid]

    it "does not return a bookmark missing the first term" $ do
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://a.com" "Haskell tutorial" "" t2019
        return (uid, bid)
      bids <- runDB $ search uid "youtube haskell"
      liftIO $ bids `shouldNotContain` [bid]

  -- ─── OR combinator (|) ───────────────────────────────────────────────────

  describe "OR combinator (| between terms)" $ do
    it "returns a bookmark matching only the first alternative" $ do
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://a.com" "marathon training" "" t2019
        return (uid, bid)
      bids <- runDB $ search uid "marathon|race"
      liftIO $ bids `shouldContain` [bid]

    it "returns a bookmark matching only the second alternative" $ do
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://a.com" "race day tips" "" t2019
        return (uid, bid)
      bids <- runDB $ search uid "marathon|race"
      liftIO $ bids `shouldContain` [bid]

    it "does not return a bookmark matching neither alternative" $ do
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://a.com" "cycling tips" "" t2019
        return (uid, bid)
      bids <- runDB $ search uid "marathon|race"
      liftIO $ bids `shouldNotContain` [bid]

    it "OR applies per-field with field prefix: ti:haskell|ti:python" $ do
      (uid, bHaskell, bPython, bJava) <- runDB $ do
        uid <- createTestUser
        bHaskell <- createBm uid "https://a.com" "Haskell guide" "" t2019
        bPython <- createBm uid "https://b.com" "Python guide" "" t2019
        bJava <- createBm uid "https://c.com" "Java guide" "" t2019
        return (uid, bHaskell, bPython, bJava)
      bids <- runDB $ search uid "ti:haskell|ti:python"
      liftIO $ bids `shouldContain` [bHaskell]
      liftIO $ bids `shouldContain` [bPython]
      liftIO $ bids `shouldNotContain` [bJava]

  -- ─── NOT combinator (-) ──────────────────────────────────────────────────

  describe "NOT combinator (- prefix)" $ do
    it "-term excludes a bookmark containing that term" $ do
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://a.com" "car reviews" "" t2019
        return (uid, bid)
      bids <- runDB $ search uid "-car"
      liftIO $ bids `shouldNotContain` [bid]

    it "-term includes a bookmark not containing that term" $ do
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://a.com" "bike reviews" "" t2019
        return (uid, bid)
      bids <- runDB $ search uid "-car"
      liftIO $ bids `shouldContain` [bid]

    it "-field:term excludes a bookmark where that field contains the term" $ do
      (uid, bidExcluded, bidIncluded) <- runDB $ do
        uid <- createTestUser
        bidExcluded <- createBm uid "https://a.com" "hacker news" "" t2019
        bidIncluded <- createBm uid "https://b.com" "lobste.rs" "" t2019
        return (uid, bidExcluded, bidIncluded)
      bids <- runDB $ search uid "-ti:\"hacker news\""
      liftIO $ bids `shouldNotContain` [bidExcluded]
      liftIO $ bids `shouldContain` [bidIncluded]

  -- ─── Quoted phrase ────────────────────────────────────────────────────────

  describe "quoted phrase search" $ do
    it "\"two words\" matches the exact substring" $ do
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://a.com" "tallest building in the world" "" t2019
        return (uid, bid)
      bids <- runDB $ search uid "\"tallest building\""
      liftIO $ bids `shouldContain` [bid]

    it "\"two words\" does not match when only one word is present" $ do
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://a.com" "tallest tower in the world" "" t2019
        return (uid, bid)
      bids <- runDB $ search uid "\"tallest building\""
      liftIO $ bids `shouldNotContain` [bid]

    it "\"two words\" does not match the words in reversed order" $ do
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://a.com" "building tallest" "" t2019
        return (uid, bid)
      bids <- runDB $ search uid "\"tallest building\""
      liftIO $ bids `shouldNotContain` [bid]

  -- ─── Complex combined queries ─────────────────────────────────────────────

  describe "complex combined queries" $ do
    -- docs example 1: u:youtube ti:haskell|ti:python a:12/31/2017
    it "u:youtube ti:haskell|ti:python a:12/31/2017 matches youtube+haskell after 2017" $ do
      let t2018 = UTCTime (fromGregorian 2018 1 1) 0
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://youtube.com/haskell" "Haskell intro" "" t2018
        return (uid, bid)
      bids <- runDB $ search uid "u:youtube ti:haskell|ti:python a:12/31/2017"
      liftIO $ bids `shouldContain` [bid]

    it "u:youtube ti:haskell|ti:python a:12/31/2017 matches youtube+python after 2017" $ do
      let t2018 = UTCTime (fromGregorian 2018 1 1) 0
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://youtube.com/python" "Python intro" "" t2018
        return (uid, bid)
      bids <- runDB $ search uid "u:youtube ti:haskell|ti:python a:12/31/2017"
      liftIO $ bids `shouldContain` [bid]

    it "u:youtube ti:haskell|ti:python a:12/31/2017 excludes youtube+java" $ do
      let t2018 = UTCTime (fromGregorian 2018 1 1) 0
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://youtube.com/java" "Java intro" "" t2018
        return (uid, bid)
      bids <- runDB $ search uid "u:youtube ti:haskell|ti:python a:12/31/2017"
      liftIO $ bids `shouldNotContain` [bid]

    it "u:youtube ti:haskell|ti:python a:12/31/2017 excludes haskell bookmark before 2018" $ do
      let t2017 = UTCTime (fromGregorian 2017 1 1) 0
      (uid, bid) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://youtube.com/haskell" "Haskell intro" "" t2017
        return (uid, bid)
      bids <- runDB $ search uid "u:youtube ti:haskell|ti:python a:12/31/2017"
      liftIO $ bids `shouldNotContain` [bid]

    -- docs example 2: -ti:"hacker news" news|cnn|npr|d:"the guardian"
    it "-ti:\"hacker news\" news|cnn excludes hacker news title" $ do
      (uid, bidExcluded, bidIncluded) <- runDB $ do
        uid <- createTestUser
        bidExcluded <- createBm uid "https://news.ycombinator.com" "hacker news" "" t2019
        bidIncluded <- createBm uid "https://cnn.com" "CNN" "" t2019
        return (uid, bidExcluded, bidIncluded)
      bids <- runDB $ search uid "-ti:\"hacker news\" news|cnn"
      liftIO $ bids `shouldNotContain` [bidExcluded]
      liftIO $ bids `shouldContain` [bidIncluded]
