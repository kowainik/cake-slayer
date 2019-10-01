{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module introduces the type class 'Has'.

module CakeSlayer.Has
       ( Has (..)
       , grab

       , Field (..)
       ) where

import GHC.Records (HasField (getField))
import GHC.TypeLits (Symbol)


{- | General type class representing which @field@ is in @env@.

Assuming that you have the following environment data type:

@
__data__ Env = Env
    { envJwtSecret :: !JwtSecret
    , ...
    }
@

Then you should first write instances for each field in the following manner:

@
__instance__ 'Has' JwtSecret Env __where__
    'obtain' :: Env -> JwtSecret
    'obtain' = envJwtSecret
@

After performing that actions, instead of plain usage like this:

@
foo :: MonadReader Env m => ...
foo = do
    secret <- asks jwtSecret
@

you could use 'Has' type class like this:

@
foo :: (MonadReader env m, Has JwtSecret m) => ...
foo = do
    secret <- asks $ 'obtain' @JwtSecret
@

and instead of @asks + obtain@ use utility function 'grab':

@
foo :: (MonadReader env m, Has JwtSecret m) => ...
foo = do
    secret <- 'grab' @JwtSecret
@
-}
class Has field env where
    obtain :: env -> field


-- | General function to retrieve fields with 'Has' class.
grab :: forall field env m . (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
{-# INLINE grab #-}


{- | The newtype for deriving via mechanism for 'Has' class. Let's see how
it's supposed to be used.

Imagine that we have a data type like this:

@
__data__ Env = Env
    { envInt :: !'Int'
    , envString :: !'String'
    }
@

GHC generates the following 'HasField' instances automatically:

@
__instance__ 'HasField' "envInt" Env 'Int' __where__
    'getField' :: Env -> 'Int'
    'getField' = envInt

__instance__ 'HasField' "envString" Env 'String' __where__
    'getField' :: Env -> 'String'
    'getField' = envString
@

You can now see that using @'getField' \@\"envInt\"@ is equivalent to @envInt@.

So, that means that it's possible to improve 'Has' instances interface and remove some boilerplate.

Instead of writing the following code:

@
__data__ Env = Env
    { envInt :: !'Int'
    , envString :: !'String'
    }

__instance__ 'Has' 'Int' Env __where__
    'obtain' :: Env -> 'Int'
    'obtain' = envInt
    \{\-\# INLINE 'obtain' \#\-\}

__instance__ 'Has' 'String' Env __where__
    'obtain' :: Env -> 'String'
    'obtain' = envString
    \{\-\# INLINE 'obtain' \#\-\}
@

It should be possible to write it using @DerivingVia@ extension:

@
__data__ Env = Env
    { envInt :: !'Int'
    , envString :: !'String'
    } __deriving__ ('Has' Int)    __via__ 'Field' "envInt" 'Int' Env
      __deriving__ ('Has' String) __via__ 'Field' "envString" 'String' Env
@

__ NOTE:__ This only works starting with @GHC-8.6@.
-}
newtype Field (s :: Symbol) field env = Field
    { unField :: env
    }

instance forall s f env . (HasField s env f) => Has f (Field s f env) where
    obtain :: Field s f env -> f
    obtain = getField @s . unField
    {-# INLINE obtain #-}
