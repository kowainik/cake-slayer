{- | This module implements the main monad for your application.

It should be used in the following way:

1. Create your custom environment data type.
2. Implement 'CakeSlayer.Has.Has' instances for your environment.
3. Specialize 'App' to your environment.
4. Implement desired effects for your specialized version of monad.
-}

module CakeSlayer.Monad
       ( App (..)
       , runApp
       , runAppAsIO
       ) where

import Control.Exception (catch, throwIO, try)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Prometheus (MonadMonitor (..))
import Relude.Extra.Bifunctor (firstF)

import CakeSlayer.Error (AppException (..), ErrorWithSource)


{- | Main application monad. It has the following type variables:

* @err@: phantom type varible that represents type of errors thrown by 'App'
* @env@: application environment that stores settings and in-memory caches
* @a@: monadic result

Specialize in your code your monomorphic environment data type like in the
examples below:

@
__type__ App     = CakeSlayer.'App' AppError  AppEnv
__type__ MockApp = CakeSlayer.'App' TestError MockEnv
@

Alternatively you can create your own @__newtype__@ if you have problems with
mutually recursive type aliases or orphan instances.

@
__newtype__ App a = App
    { unApp :: CakeSlayer.'App' AppError AppEnv a
    } __deriving newtype__ ( 'Functor', 'Applicative', 'Monad', 'MonadIO'
                       , 'MonadUnliftIO', 'MonadReader' AppEnv
                       , 'MonadError' ('ErrorWithSource' AppError)
                       )
@
-}
newtype App (err :: Type) env a = App
    { unApp :: ReaderT env IO a
    } deriving newtype ( Functor, Applicative, Monad, MonadFail
                       , MonadReader env, MonadIO, MonadUnliftIO)

{- | This instance allows to throw and catch errors that are visible in type
definitions. The implementation relies on underlying 'IO' machinery.

Use 'CakeSlayer.Error.throwError' and 'CakeSlayer.Error.catchError': these
functions automatically attach source code positions to errors.
-}
instance (Show err, Typeable err)
    => MonadError (ErrorWithSource err) (App err env)
  where
    throwError :: ErrorWithSource err -> App err env a
    throwError = liftIO . throwIO . AppException
    {-# INLINE throwError #-}

    catchError
        :: App err env a
        -> (ErrorWithSource err -> App err env a)
        -> App err env a
    catchError action handler = App $ ReaderT $ \env -> do
        let ioAction = runApp env action
        ioAction `catch` \(AppException e) -> runApp env $ handler e
    {-# INLINE catchError #-}

-- | This instances is required for the @prometheus-client@ library.
instance MonadMonitor (App err env) where
    doIO :: IO () -> App err env ()
    doIO = liftIO
    {-# INLINE doIO #-}

{- | Run application by providing environment.

Throws 'AppException' if application has unhandled 'throwError'. Use
'runAppAsIO' to handle exceptions as well.
-}
runApp :: env -> App err env a -> IO a
runApp env = usingReaderT env . unApp
{-# INLINE runApp #-}

{- | Like 'runApp' but also catches 'AppException' and unwraps 'ErrorWithSource'
from it. Use this function to handle errors outside 'App' monad.
-}
runAppAsIO
    :: (Show err, Typeable err)
    => env
    -> App err env a
    -> IO (Either (ErrorWithSource err) a)
runAppAsIO env = firstF unAppException . try . runApp env
{-# INLINE runAppAsIO #-}

