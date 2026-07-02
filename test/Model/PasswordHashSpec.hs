{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.PasswordHashSpec (spec) where

import Model.Custom
  ( HashAlgoConfig (..),
    PasswordHash (..),
    bcryptPolicy,
    hashPasswordBCrypt,
    hashPasswordBCryptWithPolicy,
    hashPasswordWith,
    needsRehash,
    validatePasswordHash,
  )
import TestImport
import Types (DB)

-- * Helpers

insertUser :: PasswordHash -> DB (Key User)
insertUser pwHash = insert $ User "alice" pwHash Nothing False False True False False Nothing

fetchStoredHash :: DB (Maybe PasswordHash)
fetchStoredHash = fmap (userPasswordHash . entityVal) <$> getBy (UniqueUserName "alice")

bcryptTarget :: HashAlgoConfig
bcryptTarget = HashAlgoBCrypt bcryptPolicy

-- * Spec

spec :: Spec
spec = do
  -- Pure/IO tests: no database needed.

  describe "hashPasswordBCrypt" $ do
    it "produces a BCrypt hash string" $ do
      h <- hashPasswordBCrypt "secret"
      unPasswordHash h `shouldSatisfy` isPrefixOf "$2"

    it "uses a random salt (same password yields different hashes)" $ do
      h1 <- hashPasswordBCrypt "secret"
      h2 <- hashPasswordBCrypt "secret"
      h1 `shouldNotBe` h2

    it "matches hashPasswordWith (HashAlgoBCrypt _)" $ do
      h <- hashPasswordWith bcryptTarget "secret"
      unPasswordHash h `shouldSatisfy` isPrefixOf "$2"

  describe "validatePasswordHash / BCrypt" $ do
    it "accepts the correct password" $ do
      h <- hashPasswordBCryptWithPolicy bcryptTestPolicy "correct"
      validatePasswordHash h "correct" `shouldBe` True

    it "rejects a wrong password" $ do
      h <- hashPasswordBCryptWithPolicy bcryptTestPolicy "correct"
      validatePasswordHash h "wrong" `shouldBe` False

  describe "validatePasswordHash / unknown format" $
    it "rejects an unrecognised hash string" $
      validatePasswordHash (PasswordHash "notahash") "anything" `shouldBe` False

  describe "needsRehash" $ do
    it "returns False for a BCrypt hash matching the target algorithm" $ do
      h <- hashPasswordBCryptWithPolicy bcryptTestPolicy "pass"
      needsRehash bcryptTarget h `shouldBe` False

    it "returns True for a hash in an unrecognised format" $
      needsRehash bcryptTarget (PasswordHash "notahash") `shouldBe` True

  -- Integration tests: require the full app + database.

  describe "authenticatePassword" $ withApp $ do
    it "succeeds for a user with a BCrypt hash" $ do
      void $ runDB $ do
        pw <- liftIO (hashPasswordBCryptWithPolicy bcryptTestPolicy "pass")
        insertUser pw
      mUser <- runDB $ authenticatePassword bcryptTarget "alice" "pass"
      liftIO $ mUser `shouldSatisfy` isJust

    it "fails with a wrong password" $ do
      void $ runDB $ do
        pw <- liftIO (hashPasswordBCryptWithPolicy bcryptTestPolicy "pass")
        insertUser pw
      mUser <- runDB $ authenticatePassword bcryptTarget "alice" "wrong"
      liftIO $ mUser `shouldSatisfy` isNothing

    it "returns Nothing for a non-existent user" $ do
      mUser <- runDB $ authenticatePassword bcryptTarget "nobody" "pass"
      liftIO $ mUser `shouldSatisfy` isNothing

    it "does not rehash an already-current BCrypt hash on successful login" $ do
      void $ runDB $ do
        pw <- liftIO (hashPasswordBCryptWithPolicy bcryptTestPolicy "pass")
        insertUser pw
      before' <- runDB fetchStoredHash
      _ <- runDB $ authenticatePassword bcryptTarget "alice" "pass"
      after' <- runDB fetchStoredHash
      liftIO $ after' `shouldBe` before'
