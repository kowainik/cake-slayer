-- | User passwords. Uses @bcrypt@ password hashing.

module CakeSlayer.Password
       ( PasswordHash (unPasswordHash)
       , PasswordPlainText (..)
       , unsafePwdHash
       , mkPasswordHashWithPolicy
       , mkPasswordHash
       , verifyPassword
       , generateRandomPassword
       ) where

import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Elm (Elm)

import CakeSlayer.Random (mkRandomString)

import qualified Crypto.BCrypt as BC


-- | Password hash.
newtype PasswordHash = PasswordHash
    { unPasswordHash :: Text
    } deriving stock (Show, Generic)
      deriving newtype (Eq, FromField, ToField)

{- | Unsafe function for constructing 'PasswordHash'.
Should be used only for testing.
-}
unsafePwdHash :: Text -> PasswordHash
unsafePwdHash = PasswordHash

-- | Password in plain text.
newtype PasswordPlainText = PasswordPlainText
    { unPasswordPlainText :: Text
    } deriving stock (Show, Generic)
      deriving newtype (Eq, FromJSON, ToJSON)
      deriving anyclass (Elm)


{- | Generates a password hash given the hashing policy and its plane text.
This has to be done in 'MonadIO' as generating the salt requires RNG.

The fast 'BC.HashingPolicy' ('BC.fastBcryptHashingPolicy') should be used
for tests only. For production use 'BC.slowerBcryptHashingPolicy', or just
'mkPasswordHash' function that already implies this.
-}
mkPasswordHashWithPolicy
    :: forall m . (MonadIO m)
    => BC.HashingPolicy
    -> PasswordPlainText
    -> m (Maybe PasswordHash)
mkPasswordHashWithPolicy hashPolicy password =
    PasswordHash . decodeUtf8 <<$>> hashBS
  where
    hashBS :: m (Maybe ByteString)
    hashBS = liftIO $ BC.hashPasswordUsingPolicy
        hashPolicy
        (encodeUtf8 $ unPasswordPlainText password)

-- | Generates the password hash with slow hashing policy.
mkPasswordHash
    :: (MonadIO m)
    => PasswordPlainText
    -> m (Maybe PasswordHash)
mkPasswordHash = mkPasswordHashWithPolicy BC.slowerBcryptHashingPolicy
{-# INLINE mkPasswordHash #-}

{- | Verifies that given 'PasswordPlainText' satisfies the 'PasswordHash'.
-}
verifyPassword :: PasswordPlainText -> PasswordHash -> Bool
verifyPassword (PasswordPlainText password) (PasswordHash hash) =
    BC.validatePassword (encodeUtf8 hash) (encodeUtf8 password)
{-# INLINE verifyPassword #-}

-- | Generate random password of the given length.
generateRandomPassword :: MonadIO m => Int -> m PasswordPlainText
generateRandomPassword n = PasswordPlainText <$> mkRandomString n
{-# INLINE generateRandomPassword #-}
