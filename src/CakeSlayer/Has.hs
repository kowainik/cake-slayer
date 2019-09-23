{-# LANGUAGE MultiParamTypeClasses #-}

{- | This module introduces the type class 'Has'.
-}

module CakeSlayer.Has
       ( Has (..)
       , grab
       ) where


{- | General type class representing which @field@ is in @env@.

Instead of plain usage like this:

@
foo :: MonadReader Env m => ...
foo = do
    secret <- asks jwtSecret
@

you should use 'Has' type class like this:

@
foo :: (MonadReader env m, Has JwtSecret m) => ...
foo = do
    secret <- asks $ obtain @JwtSecret
@

and instead of @asks + obtain@ use utility function 'grab':

@
foo :: (MonadReader env m, Has JwtSecret m) => ...
foo = do
    secret <- grab @JwtSecret
@
-}
class Has field env where
    obtain :: env -> field


-- | General function to retrieve fields with 'Has' class.
grab :: forall field env m . (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
{-# INLINE grab #-}
