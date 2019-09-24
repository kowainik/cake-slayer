{- | General-purpose generators.
-}

module Test.Gen
       ( genInt
       , genText
       ) where

import Hedgehog (Gen)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


genInt :: Gen Int
genInt = Gen.enumBounded

genText :: Gen Text
genText = Gen.text (Range.constant 1 100) Gen.alphaNum
