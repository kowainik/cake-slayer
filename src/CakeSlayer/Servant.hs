{-# LANGUAGE DataKinds #-}

{- | This module introduce aliases to use for @servant-generic@ types and
functions writing.
-}

module CakeSlayer.Servant
       ( AppServer
       , ToApi
       , RequiredHeader
       ) where

import Servant.API (Header', Required, Strict)
import Servant.API.Generic (ToServantApi)
import Servant.Server.Generic (AsServerT)


{- | A type alias for specifying  the app.

Should be used qualified to be applied to your app:

@
__import__ qualified CakeSlayer

__type__ MyAppServer = CakeSlayer.AppServer App
@
-}
type AppServer app = AsServerT app

-- | Simplified alias for 'ToServantApi'.
type ToApi (site :: Type -> Type) = ToServantApi site

-- | Non-optional header for strictly parsed text.
type RequiredHeader = Header' [Required, Strict]
