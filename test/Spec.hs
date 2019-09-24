module Main (main) where

import Hedgehog (Group (..), checkParallel)
import System.IO (hSetEncoding, utf8)

import Test.Password (pwdHashVerify)


hedgehogTests :: Group
hedgehogTests = Group "Roundtrip properties"
    [ pwdHashVerify `named` "verify . hash ≡ True"
    ]
  where
    named :: a -> b -> (b, a)
    named = flip (,)

main :: IO ()
main =  do
    -- fix terminal encoding
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

    ifM (checkParallel hedgehogTests) exitSuccess exitFailure
