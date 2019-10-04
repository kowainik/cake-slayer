{- | Utilities for generating random strings.
-}

module CakeSlayer.Random
       ( mkRandomDigits
       , mkRandomString
       ) where

import Relude.Unsafe ((!!))
import System.Random (newStdGen, randomRIO, randomRs)


-- | Generates @n@ random digits.
mkRandomDigits :: (MonadIO m) => Int -> m Text
mkRandomDigits len = toText . take len . randomRs ('0', '9') <$> liftIO newStdGen

{- | Make a random string comprised of the following letters of a given length:

1. Lowercase characters @[a..z]@
2. Uppercase characters @[A..Z]@
3. Digits @[0..9]@.

Returns empty string if given length is less than zero.
-}
mkRandomString
    :: MonadIO m
    => Int  -- ^ Length of the string
    -> m Text  -- ^ Generated string of the given length
mkRandomString n =
    liftIO $ toText <$> replicateM n peekRandomChar
  where
    alphabet :: String
    alphabet = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

    alphabetLength :: Int
    alphabetLength = length alphabet

    peekRandomChar :: IO Char
    peekRandomChar = do
        i <- randomRIO (0, alphabetLength - 1)
        pure $ alphabet !! i
