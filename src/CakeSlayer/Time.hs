{- | Helper functions and data types to work with time,
including 'UTCTime' helpers.
-}

module CakeSlayer.Time
       ( -- * 'Seconds' related functions
         Seconds (..)
       , dayInSeconds
       , threadDelay

         -- * UTCTime helpers
       , secondsElapsedInDay
       , currentSecond
       , currentMinute
       , currentHour
       ) where

import Data.Time.Clock (UTCTime (..))

import qualified Control.Concurrent as C (threadDelay)


-- | Represents the amount of seconds.
newtype Seconds = Seconds
    { unSeconds :: Int
    } deriving (Show, Eq)


-- | Similar to 'C.threadDelay' but receives 'Seconds' instead of 'Int'.
threadDelay :: MonadIO m => Seconds -> m ()
threadDelay (Seconds s) = liftIO $ C.threadDelay (s * 10 ^ (6 :: Int))

-- | Number of 'Second's in a day
dayInSeconds :: Seconds
dayInSeconds = Seconds $ 60 * 60 * 24
{-# INLINE dayInSeconds #-}

-- | Number of elapsed seconds of the given date returned as 'Int'.
secondsElapsedInDay :: UTCTime -> Int
secondsElapsedInDay = floor . toRational . utctDayTime
{-# INLINE secondsElapsedInDay #-}

-- | Number of elapsed seconds of the given date.
currentSecond :: UTCTime -> Int
currentSecond (secondsElapsedInDay -> t) = (t `mod` 3600) `mod` 60
{-# INLINE currentSecond #-}

-- | Number of elapsed minutes of the given date.
currentMinute :: UTCTime -> Int
currentMinute (secondsElapsedInDay -> t) = (t `mod` 3600) `div` 60
{-# INLINE currentMinute #-}

-- | Number of elapsed hours of the given date.
currentHour :: UTCTime -> Int
currentHour (secondsElapsedInDay -> t) = t `div` 3600
{-# INLINE currentHour #-}
