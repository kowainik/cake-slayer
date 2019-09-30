{- | Mock monad for testing purposes.
-}

module Test.Mock
       ( MockApp
       , runMockApp
       ) where

import CakeSlayer.Has (Has (..), grab)
import CakeSlayer.Jwt (JwtSecret (..), MonadJwt (..), decodeIntIdPayload, encodeIntIdPayload,
                       mkJwtTokenImpl, verifyJwtTokenImpl)
import CakeSlayer.Monad (App, runApp)


-- | Mock monad.
type MockApp = App () MockEnv

instance MonadJwt Int MockApp where
    mkJwtToken expiry payload = do
        secret <- grab @JwtSecret
        mkJwtTokenImpl encodeIntIdPayload secret expiry payload

    verifyJwtToken token = do
        secret <- grab @JwtSecret
        verifyJwtTokenImpl decodeIntIdPayload secret token

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
