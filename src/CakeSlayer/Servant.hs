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


type AppServer app = AsServerT app
type ToApi (site :: Type -> Type) = ToServantApi site

-- | Non-optional header for strictly parsed text.
type RequiredHeader = Header' [Required, Strict]
