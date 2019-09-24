module Test.Password
       ( pwdHashVerify
       ) where

import Hedgehog (Gen, Property, assert, failure, forAll, property)

import CakeSlayer.Password (PasswordPlainText (..), mkPasswordHashWithPolicy, verifyPassword)
import Test.Gen (genText)

import qualified Crypto.BCrypt as BC


pwdHashVerify :: Property
pwdHashVerify = property $ do
    randomPwd <- forAll genPwd
    pwdHash <- mkPasswordHashWithPolicy BC.fastBcryptHashingPolicy randomPwd
    case pwdHash of
        Nothing -> failure
        Just h  -> assert $ verifyPassword randomPwd h

genPwd :: Gen PasswordPlainText
genPwd = PasswordPlainText <$> genText
