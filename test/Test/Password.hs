module Test.Password
       ( pwdHashVerify
       ) where

import Hedgehog (MonadGen, Property, assert, forAll, property)

import CakeSlayer.Password (PasswordPlainText (..), mkPasswordHashWithPolicy, verifyPassword)

import qualified Crypto.BCrypt as BC
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


pwdHashVerify :: Property
pwdHashVerify = property $ do
    randomPwd <- forAll genPwd
    let hashPwd = mkPasswordHashWithPolicy BC.fastBcryptHashingPolicy randomPwd
    whenJustM hashPwd $ \pwdHash -> assert $ verifyPassword randomPwd pwdHash

genPwd :: MonadGen m => m PasswordPlainText
genPwd = PasswordPlainText <$> Gen.text (Range.constant 8 40) Gen.alphaNum
