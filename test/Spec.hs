module Main (main) where

import Hedgehog (Group (..), checkParallel)

import CakeSlayer.Jwt (decodeIntIdPayload, decodeTextIdPayload, encodeIntIdPayload,
                       encodeTextIdPayload)
import Test.Gen (genInt, genText)
import Test.Jwt (createAndVerifyJwt, jwtRoundtrip)
import Test.Password (pwdHashVerify)


hedgehogTests :: Group
hedgehogTests = Group "Roundtrip properties"
    [ "verify     . hash           ≡ True" `named` pwdHashVerify
    , "fromJwtMap . toJwtMap @Int  ≡ Just" `named`
        jwtRoundtrip genInt encodeIntIdPayload decodeIntIdPayload
    , "fromJwtMap . toJwtMap @Text ≡ Just" `named`
        jwtRoundtrip genText encodeTextIdPayload decodeTextIdPayload
    , "verifyJwt  . createJwt      ≡ True" `named` createAndVerifyJwt
    ]
  where
    named :: a -> b -> (a, b)
    named = (,)

main :: IO ()
main =
    ifM (checkParallel hedgehogTests) exitSuccess exitFailure
