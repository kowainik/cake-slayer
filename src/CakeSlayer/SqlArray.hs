{- | Introduces a useful wrapper around lists for easier work with DB
and frontend.
-}

module CakeSlayer.SqlArray
       ( SqlArray (..)
       ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger.Schema (ToSchema (..))
import Database.PostgreSQL.Simple.FromField (FieldParser, FromField (..))
import Database.PostgreSQL.Simple.ToField (Action, ToField (..))
import Database.PostgreSQL.Simple.Types (PGArray (..))
import Elm (Elm (..))


{- | Our own wrapper around list of elements. This data type is required,
because @postgresql-simple@ library doesn't have 'FromField' instance for list.
But 'PGArray' type doesn't have required instances (json, elm). So we introduce
our own type, transparent for frontend.
-}
newtype SqlArray a = SqlArray
    { unSqlArray :: [a]
    } deriving stock (Generic, Show)
      deriving newtype (Eq, FromJSON, ToJSON)

instance Elm a => Elm (SqlArray a) where
    toElmDefinition _ = toElmDefinition (Proxy @[a])

instance ToSchema a => ToSchema (SqlArray a) where
    declareNamedSchema _ = declareNamedSchema (Proxy @[a])

instance ToField a => ToField (SqlArray a) where
    toField :: SqlArray a -> Action
    toField = coerce @(PGArray a -> Action) toField

instance (FromField a, Typeable a) => FromField (SqlArray a) where
    fromField :: FieldParser (SqlArray a)
    fromField = coerce @(FieldParser (PGArray a)) fromField
