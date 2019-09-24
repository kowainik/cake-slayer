{- | Roundtrip tests for JWT.
-}

module Test.Jwt
       ( jwtRoundtrip
       ) where

import Hedgehog (Gen, Property, forAll, property, tripping)

import CakeSlayer.Jwt (JwtPayload (..))

import qualified Web.JWT as Jwt


jwtRoundtrip
    :: (Eq a, Show a)
    => Gen a  -- ^ Payload generator
    -> (JwtPayload a -> Jwt.ClaimsMap)  -- ^ Encoder
    -> (Jwt.ClaimsMap -> Maybe (JwtPayload a))  -- ^ Decoder
    -> Property
jwtRoundtrip gen encode decode = property $ do
    randomPayload <- JwtPayload <$> forAll gen
    tripping randomPayload encode decode
