{- | Wrappers around [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple)
and [postgresql-simple-named](https://hackage.haskell.org/package/postgresql-simple-named) libraries.
Instead of working with the database connection as an argument, functions in
this module work with 'MonadReader' that has access to environment with pool of
connections.
-}

module CakeSlayer.Db
       ( -- * Database pool
         DbPool
       , WithDb
       , initialisePool

         -- * Sql functions
       , queryRaw
       , query
       , queryNamed
       , queryWith
       , queryWithNamed
       , executeRaw
       , executeRaw_
       , execute
       , execute_
       , executeMany
       , executeMany_
       , executeNamed
       , executeNamed_
       , returning

         -- * Migrations
       , runMigrations

         -- * Error helpers
       , asSingleRowWith
       , failParsingWith

         -- * Deriving helpers
       , JsonField (..)

         -- * Internal helpers
       , withPool
       ) where

import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import PgNamed (NamedParam, PgNamedError)

import CakeSlayer.Error (WithError, liftError, throwOnNothingM, toNoSourceException)
import CakeSlayer.Has (Has, grab)

import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Sql
import qualified Database.PostgreSQL.Simple.FromField as Sql
import qualified Database.PostgreSQL.Simple.FromRow as Sql
import qualified Database.PostgreSQL.Simple.Migration as Sql
import qualified Database.PostgreSQL.Simple.ToField as Sql
import qualified PgNamed as Sql

----------------------------------------------------------------------------
-- Pool
----------------------------------------------------------------------------

-- | Constraint for monadic actions that wants access to database.
type WithDb env m = (MonadReader env m, Has DbPool env, MonadIO m)

-- | Pool of PostgreSQL database connections.
type DbPool = Pool.Pool Sql.Connection

{- | Create 'Pool.Pool' by given credentials. Use this function like this:

@
'initialisePool' "host=localhost port=5432 user=root dbname=my-db"
@
-}
initialisePool :: ByteString -> IO DbPool
initialisePool credentials =
    Pool.createPool (Sql.connectPostgreSQL credentials) Sql.close 10 5 10

----------------------------------------------------------------------------
-- SQL functions
----------------------------------------------------------------------------

{- | Wrapper for the 'Sql.query_' function.

Performs a given raw query without arguments and returns the resulting rows.

__Example:__

@
getUserIds :: ('WithDb' env m) => m [Only Id]
getUserIds = 'queryRaw' [sql|
    __SELECT__ id
    __FROM__ users
    |]
@
-}
queryRaw
    :: forall res env m .
       (WithDb env m, FromRow res)
    => Sql.Query
    -> m [res]
queryRaw q = withPool $ \conn -> Sql.query_ conn q
{-# INLINE queryRaw #-}

{- | Wrapper for the 'Sql.query' function.

Performs a query with the arguments and returns the resulting rows.

__Example:__

@
getJohnUserIds :: ('WithDb' env m) => m [Only Id]
getJohnUserIds = 'query' [sql|
    __SELECT__ id
    __FROM__ users
    __WHERE__ name = ?
    |] (Only \"John\")
@
-}
query
    :: forall res args env m .
       (WithDb env m, ToRow args, FromRow res)
    => Sql.Query
    -> args
    -> m [res]
query q args = withPool $ \conn -> Sql.query conn q args
{-# INLINE query #-}

{- | Wrapper for the 'Sql.queryNamed' function.
Similar to 'query' but works with 'NamedParam' instead.

You can find more about named parameters in the @postgresql-simple-named@ library:

* <https://hackage.haskell.org/package/postgresql-simple-named>

__Example:__

@
getJohnUserIds :: ('WithError' m, 'WithDb' env m) => m [Only Id]
getJohnUserIds = 'queryNamed' [sql|
    __SELECT__ id
    __FROM__ users
    __WHERE__ name = ?name
    |] [ "name" '=?' \"John\" ]
@
-}
queryNamed
    :: (WithError PgNamedError m, WithDb env m, FromRow res)
    => Sql.Query
    -> [NamedParam]
    -> m [res]
queryNamed q params =
    withPool (\conn -> runExceptT $ Sql.queryNamed conn q params) >>= liftError
{-# INLINE queryNamed #-}

{- | Wrapper for the 'Sql.queryWith' function.

Performs a query with parameters and custom 'RowParser' and returns a
list of rows.
-}
queryWith
    :: (WithDb env m, ToRow args)
    => Sql.RowParser res
    -> Sql.Query
    -> args
    -> m [res]
queryWith rowParser q params =
    withPool $ \conn -> Sql.queryWith rowParser conn q params
{-# INLINE queryWith #-}

{- | Wrapper for the 'Sql.queryWithNamed' function.

Performs a query with named parameters and custom 'RowParser' and returns a
list of rows.
-}
queryWithNamed
    :: (WithError PgNamedError m, WithDb env m)
    => Sql.RowParser res
    -> Sql.Query
    -> [NamedParam]
    -> m [res]
queryWithNamed rowParser q params =
    withPool (\conn -> runExceptT $ Sql.queryWithNamed rowParser conn q params) >>= liftError
{-# INLINE queryWithNamed #-}

{- | Wrapper for the 'Sql.execute_' function.

Executes a query without arguments that and return a number of affected rows.
-}
executeRaw
    :: (WithDb env m)
    => Sql.Query
    -> m Int64
executeRaw q = withPool $ \conn -> Sql.execute_ conn q
{-# INLINE executeRaw #-}

{- | Similar to 'executeRaw' but returns '()' instead.

Executes a query without arguments that is not expected to return results.
-}
executeRaw_
    :: (WithDb env m)
    => Sql.Query
    -> m ()
executeRaw_ = void . executeRaw
{-# INLINE executeRaw_ #-}

{- | Wrapper for the 'Sql.execute' function.

Executes a query with parameters and return a number of affected rows.
-}
execute
    :: forall args env m .
       (WithDb env m, ToRow args)
    => Sql.Query
    -> args
    -> m Int64
execute q args = withPool $ \conn -> Sql.execute conn q args
{-# INLINE execute #-}

{- | Similar to 'execute' but returns '()' instead.

Executes a query with parameters that is not expected to return results.
-}
execute_
    :: forall args env m .
       (WithDb env m, ToRow args)
    => Sql.Query
    -> args
    -> m ()
execute_ q args = void $ execute q args
{-# INLINE execute_ #-}

{- | Wrapper for the 'Sql.executeMany' function.

Executes a multi-row query and returns a number of affected rows.
-}
executeMany
    :: (WithDb env m, ToRow args)
    => Sql.Query
    -> [args]
    -> m Int64
executeMany q args = withPool $ \conn -> Sql.executeMany conn q args
{-# INLINE executeMany #-}

{- | Similar to 'executeMany' but returns '()' instead.

Executes a multi-row query that is not expected to return results.
-}
executeMany_
    :: (WithDb env m, ToRow args)
    => Sql.Query
    -> [args]
    -> m ()
executeMany_ q args = void $ executeMany q args
{-# INLINE executeMany_ #-}

{- | Wrapper for the 'Sql.executeNamed' function.

Executes a query with named parameters, returning the
number of rows affected
-}
executeNamed
    :: (WithError PgNamedError m, WithDb env m)
    => Sql.Query
    -> [NamedParam]
    -> m Int64
executeNamed q params =
    withPool (\conn -> runExceptT $ Sql.executeNamed conn q params) >>= liftError
{-# INLINE executeNamed #-}

{- | Wrapper for the 'Sql.executeNamed_' function.

Executes a query with named parameters. Like 'executeNamed' but doesn't
return number of affected columns.
-}
executeNamed_
    :: (WithError PgNamedError m, WithDb env m)
    => Sql.Query
    -> [NamedParam]
    -> m ()
executeNamed_ q params =
    withPool (\conn -> runExceptT $ Sql.executeNamed_ conn q params) >>= liftError
{-# INLINE executeNamed_ #-}


{- | Wrapper for the 'Sql.returning' function.
-}
returning
    :: (WithDb env m, ToRow args, FromRow res)
    => Sql.Query
    -> [args]
    -> m [res]
returning q args = withPool $ \conn -> Sql.returning conn q args
{-# INLINE returning #-}

----------------------------------------------------------------------------
-- Migrations
----------------------------------------------------------------------------

-- | Runs SQL migrations in verbose mode.
runMigrations
    :: WithDb env m
    => [Sql.MigrationCommand]
    -> m (Sql.MigrationResult String)
runMigrations commands = withPool $ \conn -> Sql.runMigrations True conn commands

----------------------------------------------------------------------------
-- Errors
----------------------------------------------------------------------------

{- | Throws a given error if a monadic action (expected resut of SQL query)
returns no rows. Otherwise returns the first row.

@
getUserByEmail :: ('WithDb' env m, 'WithError' m) => Email -> m User
getUserByEmail email = 'asSingleRowWith' emailNotExistError $ 'queryNamed' [sql|
    __SELECT__ id, name
    __FROM__ users
    __WHERE__ email = ?email
|] [ "email" '=?' email ]
@
-}
asSingleRowWith :: WithError err m => err -> m [a] -> m a
asSingleRowWith err action = throwOnNothingM err (viaNonEmpty head <$> action)
{-# INLINE asSingleRowWith #-}

-- | Helper to throw a given when defining a 'FromRow' instance
failParsingWith :: (Show err, Typeable err) => err -> Sql.RowParser a
failParsingWith err =
    Sql.fieldWith (\_ _ -> Sql.conversionError $ toNoSourceException err)
{-# INLINE failParsingWith #-}

----------------------------------------------------------------------------
-- Deriving helpers
----------------------------------------------------------------------------

{- | Data type helper to derive 'ToField' and 'FromField' instances for @json@
and @jsonb@ PostgreSQL types using @DerivingVia@ language extensions.

Use it like this:

@
__data__ Foo = Foo
    { fooTime  :: !Int
    , fooValue :: !Text
    } __deriving__ (FromField, ToField) __via__ 'JsonField' Foo
@
-}
newtype JsonField a = JsonField
    { unJsonField :: a
    }

instance (FromJSON a, Typeable a) => Sql.FromField (JsonField a) where
    fromField sqlField mdata = JsonField <$> Sql.fromJSONField sqlField mdata
    {-# INLINE fromField #-}

instance ToJSON a => Sql.ToField (JsonField a) where
    toField = Sql.toJSONField . unJsonField
    {-# INLINE toField #-}

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Perform action that needs database connection.
withPool :: WithDb env m => (Sql.Connection -> IO b) -> m b
withPool action = do
    pool <- grab @DbPool
    liftIO $ Pool.withResource pool action
{-# INLINE withPool #-}
