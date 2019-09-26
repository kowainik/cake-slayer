{- | Roundtrip tests for JWT.
-}

module Test.Jwt
       ( jwtRoundtrip
       , createAndVerifyJwt
       ) where

import Hedgehog (Gen, Property, forAll, property, tripping, (===))

import CakeSlayer.Jwt (JwtPayload (..), MonadJwt (..))
import CakeSlayer.Time (Seconds (..))
import Test.Gen (genInt)
import Test.Mock (runMockApp)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
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

createAndVerifyJwt :: Property
createAndVerifyJwt = property $ do
    seconds <- forAll genSeconds
    payload <- forAll genPayload
    verifiedPayload <- liftIO $ runMockApp $ makeAndVerifyToken seconds payload
    verifiedPayload === Just payload

genSeconds :: Gen Seconds
genSeconds = Seconds <$> Gen.int (Range.constant 2 100)

genPayload :: Gen (JwtPayload Int)
genPayload = JwtPayload <$> genInt

makeAndVerifyToken
    :: MonadJwt Int m
    => Seconds
    -> JwtPayload Int
    -> m (Maybe (JwtPayload Int))
makeAndVerifyToken expiry = mkJwtToken expiry >=> verifyJwtToken
