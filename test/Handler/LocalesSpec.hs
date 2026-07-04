{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.LocalesSpec (spec) where

import Data.ByteString qualified as BS
import Handler.Locales (isSafePiece, isWithinDir)
import System.Directory
  ( createDirectoryIfMissing,
    getTemporaryDirectory,
    removeDirectoryRecursive,
  )
import TestImport

spec :: Spec
spec = do
  describe "Handler.Locales.isSafePiece" $ do
    it "allows ordinary locale path segments" $ do
      mapM_
        (`shouldSatisfy` isSafePiece)
        ["en", "translation.json", "pt-BR", "zh_CN", "a.b-c_9"]

    it "rejects empty segments" $
      isSafePiece "" `shouldBe` False

    it "rejects '.' and '..'" $ do
      isSafePiece "." `shouldBe` False
      isSafePiece ".." `shouldBe` False

    it "rejects anything starting with '.', including hidden files" $ do
      isSafePiece ".env" `shouldBe` False
      isSafePiece ".git" `shouldBe` False

    it "rejects segments containing a path separator" $ do
      isSafePiece "foo/bar" `shouldBe` False
      isSafePiece "foo\\bar" `shouldBe` False

    it "rejects a drive-letter colon that could hijack an absolute path join" $
      isSafePiece "C:" `shouldBe` False

    it "rejects other punctuation and whitespace outside the whitelist" $ do
      isSafePiece "foo bar" `shouldBe` False
      isSafePiece "foo;bar" `shouldBe` False
      isSafePiece "foo\0bar" `shouldBe` False

  around withScratchDir $
    describe "Handler.Locales.isWithinDir" $ do
      it "allows a file that exists inside the directory" $ \root -> do
        let dir = root </> "locales"
            file = dir </> "en" </> "translation.json"
        createDirectoryIfMissing True (dir </> "en")
        BS.writeFile file "{}"
        isWithinDir dir file `shouldReturn` True

      it "rejects a path that does not exist" $ \root -> do
        let dir = root </> "locales"
            file = dir </> "en" </> "missing.json"
        createDirectoryIfMissing True (dir </> "en")
        isWithinDir dir file `shouldReturn` False

      it "rejects a path outside the directory even without traversal syntax" $ \root -> do
        let dir = root </> "locales"
            outsideFile = root </> "secret.yml"
        createDirectoryIfMissing True dir
        BS.writeFile outsideFile "secret"
        isWithinDir dir outsideFile `shouldReturn` False

      it "rejects a sibling directory sharing a name prefix" $ \root -> do
        let dir = root </> "locales"
            evilDir = root </> "locales-evil"
            evilFile = evilDir </> "translation.json"
        createDirectoryIfMissing True dir
        createDirectoryIfMissing True evilDir
        BS.writeFile evilFile "{}"
        isWithinDir dir evilFile `shouldReturn` False
  where
    withScratchDir :: (FilePath -> IO ()) -> IO ()
    withScratchDir action = do
      tmp <- getTemporaryDirectory
      let root = tmp </> "espial-locales-spec"
      removeDirectoryRecursive root `catchAny` \_ -> pure ()
      createDirectoryIfMissing True root
      action root `finally` (removeDirectoryRecursive root `catchAny` \_ -> pure ())
