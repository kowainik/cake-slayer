{- | This module provides convenient wrappers around JWT provided by @jwt@
Haskell library. These tokens are used to authenticate users and store
additional payload on user log in, for example, user id.
-}

module CakeSlayer.Jwt
       ( -- * Secret
         JwtSecret (..)

         -- * Token and payloads
         -- ** Data types
       , JwtToken (..)
       , JwtPayload (..)
         -- ** Coders
       , encodeIntIdPayload
       , decodeIntIdPayload
       , encodeTextIdPayload
       , decodeTextIdPayload
         -- ** Helpers to write coders
       , payloadToMap
       , payloadFromMap

         -- * Jwt Effect
         -- ** Monad
       , MonadJwt (..)
         -- ** Internals of 'MonadJwt'
       , mkJwtTokenImpl
       , verifyJwtTokenImpl
       ) where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Scientific (toBoundedInteger)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Elm (Elm)
import Web.HttpApiData (FromHttpApiData)

import CakeSlayer.Has (Has, grab)
import CakeSlayer.Time (Seconds (..))

import qualified Data.Aeson as Json
import qualified Data.Map as Map
import qualified Web.JWT as Jwt

----------------------------------------------------------------------------
-- Secret
----------------------------------------------------------------------------

{- | JWT secret that is used to sign and verify JSON web tokens.

You can use functions from "CakeSlayer.Random" module to create random
'JwtSecret' at the start of your application like this:

@
jwtSecret <- 'JwtSecret' <$> mkRandomString 10
@
-}
newtype JwtSecret = JwtSecret
    { unJwtSecret :: Text
    }

----------------------------------------------------------------------------
-- Token and payload
----------------------------------------------------------------------------

{- | Encoded JSON web token.
-}
newtype JwtToken = JwtToken
    { unJwtToken :: Text
    } deriving stock (Show, Generic)
      deriving newtype (Eq, Ord, Hashable, FromField, ToField, FromHttpApiData, FromJSON, ToJSON)
      deriving anyclass Elm

{- | Stores arbitrary payload. If you want to store your custom payload, you
need to specify two functions:

1. How to encode 'JwtPayload' as 'Jwt.ClaimsMap'.
2. How to decode 'JwtPayload from 'Jwt.ClaimsMap'.

See examples in this module: 'encodeIntIdPayload', 'decodeIntIdPayload',
'encodeTextIdPayload', 'decodeTextIdPayload'.
-}
newtype JwtPayload a = JwtPayload
    { unJwtPayload :: a
    } deriving stock (Show, Functor)
      deriving newtype (Eq)

{- | Encodes 'JwtPayload' that stores 'Int' as payload with name @id@. Use it if
you store ids as integer values. Dual to 'decodeIntIdPayload'.
-}
encodeIntIdPayload :: JwtPayload Int -> Jwt.ClaimsMap
encodeIntIdPayload = payloadToMap . Json.Number . fromIntegral . unJwtPayload
{-# INLINE encodeIntIdPayload #-}

{- | Decodes 'JwtPayload' from 'Jwt.ClaimsMap' that stores 'Int' under name
@id@. Dual to 'encodeIntIdPayload'.
-}
decodeIntIdPayload :: Jwt.ClaimsMap -> Maybe (JwtPayload Int)
decodeIntIdPayload = fmap JwtPayload . payloadFromMap (\case
    Json.Number jwtId -> toBoundedInteger jwtId
    _ -> Nothing)
{-# INLINE decodeIntIdPayload #-}

{- | Encodes 'JwtPayload' that stores 'Text' as payload with name @id@. Use it if
you store ids as text or UUID values. Dual to 'decodeTextIdPayload'.
-}
encodeTextIdPayload :: JwtPayload Text -> Jwt.ClaimsMap
encodeTextIdPayload = payloadToMap . Json.String . unJwtPayload
{-# INLINE encodeTextIdPayload #-}

{- | Decodes 'JwtPayload' from 'Jwt.ClaimsMap' that stores 'Int' under name
@id@. Dual to 'encodeIntIdPayload'.
-}
decodeTextIdPayload :: Jwt.ClaimsMap -> Maybe (JwtPayload Text)
decodeTextIdPayload = fmap JwtPayload . payloadFromMap (\case
    Json.String jwtId -> Just jwtId
    _ -> Nothing)
{-# INLINE decodeTextIdPayload #-}

-- | Creates 'Jwt.ClaimsMap' from 'Json.Value' under name @id@.
payloadToMap :: Json.Value -> Jwt.ClaimsMap
payloadToMap val = Jwt.ClaimsMap $ Map.fromList [("id", val)]
{-# INLINE payloadToMap #-}

{- | Decodes payload from 'Jwt.ClaimsMap' under @id@ name given 'Json.Value'
extractor.
-}
payloadFromMap :: (Value -> Maybe a) -> Jwt.ClaimsMap -> Maybe a
payloadFromMap fromValue (Jwt.ClaimsMap claimsMap) =
    Map.lookup "id" claimsMap >>= fromValue
{-# INLINE payloadFromMap #-}

----------------------------------------------------------------------------
-- Secret
----------------------------------------------------------------------------

{- | This monad represents effect to create and verify JWT.

TODO: parametrize 'JwtPayload' when we figure out how to do this
-}
class Monad m => MonadJwt m where
    mkJwtToken
        :: Seconds          -- ^ Token expiry in seconds
        -> JwtPayload Text  -- ^ Payload to code;
        -> m JwtToken       -- ^ Encoded token

    verifyJwtToken
        :: JwtToken  -- ^ Token which stores payload
        -> m (Maybe (JwtPayload Text))  -- ^ Decoded payload if token valid

-- | Default implementation of token creation.
mkJwtTokenImpl
    :: (MonadIO m, MonadReader env m, Has JwtSecret env)
    => Seconds
    -> JwtPayload Text
    -> m JwtToken
mkJwtTokenImpl expiry payload = do
    secret  <- Jwt.hmacSecret . unJwtSecret <$> grab @JwtSecret
    timeNow <- liftIO getPOSIXTime
    let expiryTime = timeNow + fromIntegral (unSeconds expiry)
    let claimsSet = mempty
            { Jwt.exp = Jwt.numericDate expiryTime
            , Jwt.unregisteredClaims = encodeTextIdPayload payload
            }
    pure $ JwtToken $ Jwt.encodeSigned secret mempty claimsSet

-- | Default implementation of token validation.
verifyJwtTokenImpl
    :: (MonadIO m, MonadReader env m, Has JwtSecret env)
    => JwtToken
    -> m (Maybe (JwtPayload Text))
verifyJwtTokenImpl (JwtToken token) = do
    secret <- Jwt.hmacSecret . unJwtSecret <$> grab @JwtSecret
    timeNow <- Jwt.numericDate <$> liftIO getPOSIXTime
    pure $ do
        claimsSet <- Jwt.claims <$> Jwt.decodeAndVerifySignature secret token
        expiryTimeStatedInToken <- Jwt.exp claimsSet
        now <- timeNow
        guard (expiryTimeStatedInToken >= now)
        decodeTextIdPayload $ Jwt.unregisteredClaims claimsSet
