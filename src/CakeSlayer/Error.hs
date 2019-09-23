{- | This module contains types ans functions for the app error customization.
-}

module CakeSlayer.Error
       ( -- * Pure errors handling
         WithError
       , ErrorWithSource (..)
       , throwError

         -- * Exceptions
       , AppException (..)
       , toNoSourceException

         -- * Helper unctions
       , throwOnNothing
       , throwOnNothingM

         -- * 'SourcePosition' helpers
       , SourcePosition (..)
       , toSourcePosition
       ) where

import Control.Monad.Except (MonadError)
import GHC.Stack (SrcLoc (SrcLoc, srcLocModule, srcLocStartLine))

import qualified Control.Monad.Except as E (throwError)


{- | Type alias for errors that has access to 'CallStack'. Specialise this
constraint in your code to monomorphic error data type like this:

@
__type__ WithError = CakeSlayer.'WithError' MyError
@
-}
type WithError err m = (MonadError (ErrorWithSource err) m, HasCallStack)

{- | Wrapper around error type with attached source code position.
-}
data ErrorWithSource err = ErrorWithSource
    { appErrorCallStack :: !SourcePosition
    , appErrorType      :: !err
    } deriving stock (Show, Eq, Functor)

{- | Specialized version of 'E.throwError' that attaches source code position of
the place where this error was thrown.
-}
throwError :: WithError err m => err -> m a
throwError = E.throwError . ErrorWithSource (toSourcePosition callStack)
{-# INLINE throwError #-}

newtype SourcePosition = SourcePosition Text
    deriving newtype (Show, Eq)

{- | Display 'CallStack' as 'SourcePosition' in the following format:

@
Module.function#line_number
@
-}
toSourcePosition :: CallStack -> SourcePosition
toSourcePosition cs = SourcePosition showCallStack
  where
    showCallStack :: Text
    showCallStack = case getCallStack cs of
        []                             -> "<unknown loc>"
        [(name, loc)]                  -> showLoc name loc
        (_, loc) : (callerName, _) : _ -> showLoc callerName loc

    showLoc :: String -> SrcLoc -> Text
    showLoc name SrcLoc{..} =
        toText srcLocModule <> "." <> toText name <> "#" <> show srcLocStartLine

{- | Exception wrapper around 'ErrorWithSource'. Useful when you need to throw/catch
'ErrorWithSource' as 'Exception'.
-}
newtype AppException err = AppException
    { unAppException :: ErrorWithSource err
    } deriving stock (Show)
      deriving anyclass (Exception)

{- | Helper to convert 'err' into something that can be thrown
when you don't have the ability to specify the 'SourcePosition'.
-}
toNoSourceException :: err -> AppException err
toNoSourceException = AppException . ErrorWithSource (SourcePosition "<unknown loc>")
{-# INLINE toNoSourceException #-}

{- | Extract the value from a maybe, throwing the given @err@ if
the value does not exist.
-}
throwOnNothing :: WithError err m => err -> Maybe a -> m a
throwOnNothing err = withFrozenCallStack . maybe (throwError err) pure
{-# INLINE throwOnNothing #-}

{- | Extract the value from a 'Maybe' in @m@, throwing the given @err@ if
the value does not exist.
-}
throwOnNothingM :: WithError err m => err -> m (Maybe a) -> m a
throwOnNothingM err action = withFrozenCallStack $ action >>= throwOnNothing err
{-# INLINE throwOnNothingM #-}
