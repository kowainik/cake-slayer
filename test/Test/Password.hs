module Test.Password
       ( pwdHashVerify
       ) where

import Hedgehog (MonadGen, Property, assert, failure, forAll, property)

import CakeSlayer.Password (PasswordPlainText (..), mkPasswordHashWithPolicy, verifyPassword)

import qualified Crypto.BCrypt as BC
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


pwdHashVerify :: Property
pwdHashVerify = property $ do
    randomPwd <- forAll genPwd
    pwdHash <- mkPasswordHashWithPolicy BC.fastBcryptHashingPolicy randomPwd
    case pwdHash of
        Nothing -> failure
        Just h  -> assert $ verifyPassword randomPwd h

genPwd :: MonadGen m => m PasswordPlainText
genPwd = PasswordPlainText <$> Gen.text (Range.constant 8 40) Gen.alphaNum
