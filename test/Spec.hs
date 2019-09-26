module Main (main) where

import Hedgehog (Group (..), checkParallel)
import System.IO (hSetEncoding, utf8)

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
main =  do
    -- fix terminal encoding
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

    ifM (checkParallel hedgehogTests) exitSuccess exitFailure
