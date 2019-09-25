{-# LANGUAGE BangPatterns #-}

{- | This module contains the 'MonadMeasure' type class which purpose is to
apply some metric calculations on actions, e.g. taken time.
-}

module CakeSlayer.Measure
       ( MonadMeasure
       , timedAction

         -- * Internals
       , timedActionImpl
       ) where

import Prometheus (Histogram, Info (..), MonadMonitor, defaultBuckets, histogram, observe, register)
import Relude.Extra.CallStack (ownName)
import System.CPUTime (getCPUTime)
import System.Metrics.Distribution (Distribution)

import CakeSlayer.Has (Has, grab)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified System.Metrics as Metrics
import qualified System.Metrics.Distribution as Distribution

{- | This type class is used to perform action and measure time and other
metrics needed to be performed.
-}
class Monad m => MonadTimed m where
    timedAction :: HasCallStack => m a -> m a


-- QUESTION: move to CakeSlayer.Env ?
type Timings = IORef (Map Text Distribution)
type PrometheusRegistry = IORef (HashMap Text Histogram)

type MonadMeasure m = (HasCallStack, MonadTimed m)

{- | Helper function to be used in instance implementations.

It measures the time taken to perform the given action and store it
in the @timings@ distribution with the label of the action.

This is a recommended implementation which has the integration with @prometheus@.
-}
timedActionImpl
    :: forall r m a .
       ( MonadReader r m
       , Has Timings r
       , Has Metrics.Store r
       , Has PrometheusRegistry r
       , MonadIO m
       , HasCallStack
       , MonadMonitor m
       )
    => Text  -- ^ Global name
    -> m a   -- ^ Action
    -> m a
timedActionImpl nm action = do
    start <- liftIO getCPUTime
    !result <- action
    end <- liftIO getCPUTime
    let !timeTaken = fromIntegral (end - start) * 1e-12
    dist <- getOrCreateDistribution $ toText ownName
    liftIO $ Distribution.add dist timeTaken
    registerWithPrometheus (toText ownName) timeTaken
    pure result
  where
    registerWithPrometheus :: Text -> Double -> m ()
    registerWithPrometheus label reading = do
        promRef <- grab @PrometheusRegistry
        promMap <- readIORef promRef
        case HashMap.lookup label promMap of
            Just hist -> observe hist reading
            Nothing   -> do
                newHist <- register $ histogram (Prometheus.Info nm label) defaultBuckets
                () <$ atomicModifyIORef' promRef (\pHashMap -> (HashMap.insert label newHist pHashMap, ()))
                observe newHist reading


    getOrCreateDistribution :: Text -> m Distribution
    getOrCreateDistribution label = do
        timingsRef <- grab @Timings
        store      <- grab @Metrics.Store
        liftIO $ do
            distMap <- readIORef timingsRef
            whenNothing (Map.lookup label distMap) $ do
                newDist <- Metrics.createDistribution label store
                modifyIORef' timingsRef (Map.insert label newDist)
                return newDist
