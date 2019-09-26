{- | Mock monad for testing purposes.
-}

module Test.Mock
       ( MockApp
       , runMockApp
       ) where

import CakeSlayer.Has (Has (..))
import CakeSlayer.Jwt (JwtSecret (..), MonadJwt (..), decodeIntIdPayload, encodeIntIdPayload,
                       mkJwtTokenImpl, verifyJwtTokenImpl)
import CakeSlayer.Monad (App, runApp)


-- | Mock monad.
type MockApp = App () MockEnv

instance MonadJwt Int MockApp where
    mkJwtToken = mkJwtTokenImpl encodeIntIdPayload
    verifyJwtToken = verifyJwtTokenImpl decodeIntIdPayload

-- | Environment for 'MockApp'.
newtype MockEnv = MockEnv
    { mockEnvJwtSecret :: JwtSecret
    }

instance Has JwtSecret MockEnv where
    obtain = mockEnvJwtSecret

mockEnv :: MockEnv
mockEnv = MockEnv
    { mockEnvJwtSecret = JwtSecret "0123456789"
    }

runMockApp :: MockApp a -> IO a
runMockApp = runApp mockEnv
