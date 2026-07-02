{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.PasswordHashSpec (spec) where

import Model.Custom
  ( HashAlgoConfig (..),
    PasswordHash (..),
    bcryptPolicy,
    defaultArgon2idOptions,
    hashPassword,
    hashPasswordBCrypt,
    hashPasswordWith,
    mkArgon2idOptions,
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

argon2Target :: HashAlgoConfig
argon2Target = HashAlgoArgon2id defaultArgon2idOptions

bcryptTarget :: HashAlgoConfig
bcryptTarget = HashAlgoBCrypt bcryptPolicy

-- * Spec

spec :: Spec
spec = do
  -- Pure/IO tests: no database needed.

  describe "hashPassword" $ do
    it "produces an Argon2id PHC string" $ do
      h <- hashPassword "secret"
      unPasswordHash h `shouldSatisfy` isPrefixOf "$argon2id$"

    it "uses a random salt (same password yields different hashes)" $ do
      h1 <- hashPassword "secret"
      h2 <- hashPassword "secret"
      h1 `shouldNotBe` h2

  describe "validatePasswordHash / Argon2id" $ do
    it "accepts the correct password" $ do
      h <- hashPassword "correct"
      validatePasswordHash h "correct" `shouldBe` True

    it "rejects a wrong password" $ do
      h <- hashPassword "correct"
      validatePasswordHash h "wrong" `shouldBe` False

  describe "validatePasswordHash / BCrypt (legacy)" $ do
    it "accepts the correct password" $ do
      h <- hashPasswordBCrypt "correct"
      validatePasswordHash h "correct" `shouldBe` True

    it "rejects a wrong password" $ do
      h <- hashPasswordBCrypt "correct"
      validatePasswordHash h "wrong" `shouldBe` False

  describe "validatePasswordHash / unknown format" $
    it "rejects an unrecognised hash string" $
      validatePasswordHash (PasswordHash "notahash") "anything" `shouldBe` False

  describe "needsRehash" $ do
    it "returns True for a BCrypt hash" $ do
      h <- hashPasswordBCrypt "pass"
      needsRehash argon2Target h `shouldBe` True

    it "returns False for an Argon2id hash matching the target parameters" $ do
      h <- hashPassword "pass"
      needsRehash argon2Target h `shouldBe` False

    it "returns True for an Argon2id hash whose parameters differ from the target" $ do
      h <- hashPasswordWith (HashAlgoArgon2id (mkArgon2idOptions 12288 3 1)) "pass"
      needsRehash argon2Target h `shouldBe` True

    it "returns True for an Argon2id hash when the selected algo is BCrypt" $ do
      h <- hashPassword "pass"
      needsRehash bcryptTarget h `shouldBe` True

    it "returns False for a BCrypt hash when the selected algo is BCrypt" $ do
      h <- hashPasswordBCrypt "pass"
      needsRehash bcryptTarget h `shouldBe` False

  -- Integration tests: require the full app + database.

  describe "authenticatePassword" $ withApp $ do
    it "succeeds for a user with an Argon2id hash" $ do
      runDB $ do
        pw <- liftIO (hashPassword "pass")
        insertUser pw
      mUser <- runDB $ authenticatePassword argon2Target "alice" "pass"
      liftIO $ mUser `shouldSatisfy` isJust

    it "fails with a wrong password (Argon2id user)" $ do
      runDB $ do
        pw <- liftIO (hashPassword "pass")
        insertUser pw
      mUser <- runDB $ authenticatePassword argon2Target "alice" "wrong"
      liftIO $ mUser `shouldSatisfy` isNothing

    it "succeeds for a user with a BCrypt hash" $ do
      runDB $ do
        pw <- liftIO (hashPasswordBCrypt "pass")
        insertUser pw
      mUser <- runDB $ authenticatePassword argon2Target "alice" "pass"
      liftIO $ mUser `shouldSatisfy` isJust

    it "fails with a wrong password (BCrypt user)" $ do
      runDB $ do
        pw <- liftIO (hashPasswordBCrypt "pass")
        insertUser pw
      mUser <- runDB $ authenticatePassword argon2Target "alice" "wrong"
      liftIO $ mUser `shouldSatisfy` isNothing

    it "returns Nothing for a non-existent user" $ do
      mUser <- runDB $ authenticatePassword argon2Target "nobody" "pass"
      liftIO $ mUser `shouldSatisfy` isNothing

    it "rehashes BCrypt to Argon2id on successful login" $ do
      runDB $ do
        pw <- liftIO (hashPasswordBCrypt "pass")
        insertUser pw
      _ <- runDB $ authenticatePassword argon2Target "alice" "pass"
      mHash <- runDB fetchStoredHash
      liftIO $ case mHash of
        Nothing -> expectationFailure "user not found after authentication"
        Just h -> unPasswordHash h `shouldSatisfy` isPrefixOf "$argon2id$"

    it "the rehashed Argon2id password still validates correctly" $ do
      runDB $ do
        pw <- liftIO (hashPasswordBCrypt "pass")
        insertUser pw
      _ <- runDB $ authenticatePassword argon2Target "alice" "pass"
      mUser <- runDB $ authenticatePassword argon2Target "alice" "pass"
      liftIO $ mUser `shouldSatisfy` isJust

    it "does not update the stored hash when already Argon2id under the target parameters" $ do
      runDB $ do
        pw <- liftIO (hashPassword "pass")
        insertUser pw
      before' <- runDB fetchStoredHash
      _ <- runDB $ authenticatePassword argon2Target "alice" "pass"
      after' <- runDB fetchStoredHash
      liftIO $ after' `shouldBe` before'

    it "rehashes an Argon2id hash whose parameters differ from the target" $ do
      runDB $ do
        pw <- liftIO (hashPasswordWith (HashAlgoArgon2id (mkArgon2idOptions 12288 3 1)) "pass")
        insertUser pw
      _ <- runDB $ authenticatePassword argon2Target "alice" "pass"
      mHash <- runDB fetchStoredHash
      liftIO $ case mHash of
        Nothing -> expectationFailure "user not found after authentication"
        Just h -> unPasswordHash h `shouldSatisfy` isPrefixOf "$argon2id$v=19$m=19456,t=2,p=1$"

    it "rehashes Argon2id to BCrypt when the selected algo is BCrypt" $ do
      runDB $ do
        pw <- liftIO (hashPassword "pass")
        insertUser pw
      _ <- runDB $ authenticatePassword bcryptTarget "alice" "pass"
      mHash <- runDB fetchStoredHash
      liftIO $ case mHash of
        Nothing -> expectationFailure "user not found after authentication"
        Just h -> unPasswordHash h `shouldSatisfy` isPrefixOf "$2"
