{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Model.BookmarksQuerySpec (spec) where

import Model.Custom (hashPassword, mkBmSlug)
import TestImport
import Types (DB)

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2024 1 1) 0

createTestUser :: DB (Key User)
createTestUser = do
  pwHash <- liftIO $ hashPassword "pass"
  insert $ User "testuser" pwHash Nothing False False True False Nothing

createBm :: Key User -> Text -> DB (Key Bookmark)
createBm uid href = do
  slug <- liftIO mkBmSlug
  insert $ Bookmark uid slug href "desc" "" t0 False False False Nothing

tagBm :: Key User -> Key Bookmark -> Text -> Int -> DB ()
tagBm uid bid tag seq' = insert_ $ BookmarkTag uid tag bid seq'

queryBids :: Key User -> [Text] -> Maybe Text -> Bool -> DB [Key Bookmark]
queryBids uid tags mq isowner = do
  (_, rows, _, _) <- bookmarksTagsQuery uid isowner SharedAll FilterAll tags mq Nothing 100 1
  return $ map (entityKey . fst) rows

spec :: Spec
spec = withApp $ do
  -- bookmarkWhereClause line 399: `like val tag` for FilterTags
  describe "bookmarkWhereClause tag filter" $ do
    it "treats _ as a literal, not a wildcard" $ do
      (uid, bid1, _) <- runDB $ do
        uid <- createTestUser
        bid1 <- createBm uid "https://a.com"
        bid2 <- createBm uid "https://b.com"
        tagBm uid bid1 "foo_bar" 1
        tagBm uid bid2 "fooXbar" 1
        return (uid, bid1, bid2)
      bids <- runDB $ queryBids uid ["foo_bar"] Nothing True
      liftIO $ bids `shouldBe` [bid1]

    it "treats % as a literal, not a wildcard" $ do
      (uid, bid1, _) <- runDB $ do
        uid <- createTestUser
        bid1 <- createBm uid "https://a.com"
        bid2 <- createBm uid "https://b.com"
        tagBm uid bid1 "100%" 1
        tagBm uid bid2 "100x" 1
        return (uid, bid1, bid2)
      bids <- runDB $ queryBids uid ["100%"] Nothing True
      liftIO $ bids `shouldBe` [bid1]

    -- intentional: .% is used to identify private tags (starts-with-dot pattern)
    it "non-owner queries exclude tags starting with . from the display string" $ do
      (uid, _) <- runDB $ do
        uid <- createTestUser
        bid <- createBm uid "https://a.com"
        tagBm uid bid ".secret" 1
        tagBm uid bid "public" 2
        return (uid, bid)
      (_, rows, _, _) <- runDB $ bookmarksTagsQuery uid False SharedAll FilterAll [] Nothing Nothing 100 1
      let mTagStr = snd =<< listToMaybe rows
      liftIO $ mTagStr `shouldBe` Just "public"

  -- bookmarkLikeExpr lines 432/446: `like wild term` for tag search
  describe "bookmarkLikeExpr tag search" $ do
    it "tags:foo_bar treats _ as a literal, not a wildcard" $ do
      (uid, bid1, _) <- runDB $ do
        uid <- createTestUser
        bid1 <- createBm uid "https://a.com"
        bid2 <- createBm uid "https://b.com"
        tagBm uid bid1 "foo_bar" 1
        tagBm uid bid2 "fooXbar" 1
        return (uid, bid1, bid2)
      bids <- runDB $ queryBids uid [] (Just "tags:foo_bar") True
      liftIO $ bids `shouldBe` [bid1]

    it "tags:100% treats % as a literal, not a wildcard" $ do
      (uid, bid1, _) <- runDB $ do
        uid <- createTestUser
        bid1 <- createBm uid "https://a.com"
        bid2 <- createBm uid "https://b.com"
        tagBm uid bid1 "100%" 1
        tagBm uid bid2 "100x" 1
        return (uid, bid1, bid2)
      bids <- runDB $ queryBids uid [] (Just "tags:100%") True
      liftIO $ bids `shouldBe` [bid1]

  -- relatedTagWhere line 911: `like val tag` used in tagCountRelated
  describe "tagCountRelated" $ do
    it "treats _ as a literal when matching co-tagged bookmarks" $ do
      uid <- runDB $ do
        uid <- createTestUser
        bid1 <- createBm uid "https://a.com"
        bid2 <- createBm uid "https://b.com"
        tagBm uid bid1 "foo_bar" 1
        tagBm uid bid1 "related" 2
        tagBm uid bid2 "fooXbar" 1
        tagBm uid bid2 "unrelated" 2
        return uid
      related <- runDB $ tagCountRelated uid ["foo_bar"]
      let tagNames = map fst related
      liftIO $ tagNames `shouldContain` ["related"]
      liftIO $ tagNames `shouldNotContain` ["unrelated"]

    it "treats % as a literal when matching co-tagged bookmarks" $ do
      uid <- runDB $ do
        uid <- createTestUser
        bid1 <- createBm uid "https://a.com"
        bid2 <- createBm uid "https://b.com"
        tagBm uid bid1 "100%" 1
        tagBm uid bid1 "related" 2
        tagBm uid bid2 "100x" 1
        tagBm uid bid2 "unrelated" 2
        return uid
      related <- runDB $ tagCountRelated uid ["100%"]
      let tagNames = map fst related
      liftIO $ tagNames `shouldContain` ["related"]
      liftIO $ tagNames `shouldNotContain` ["unrelated"]
