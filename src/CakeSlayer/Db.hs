{- | Wrappers around @postgresq-simple@ and @postgresql-simple-named@ libraries.
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

         -- * Internal helpers
       , withPool
       ) where

import Database.PostgreSQL.Simple (FromRow, ToRow)
import PgNamed (NamedParam, PgNamedError)

import CakeSlayer.Error (WithError, liftError, throwOnNothingM, toNoSourceException)
import CakeSlayer.Has (Has, grab)

import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Sql
import qualified Database.PostgreSQL.Simple.FromField as Sql (conversionError)
import qualified Database.PostgreSQL.Simple.FromRow as Sql
import qualified Database.PostgreSQL.Simple.Migration as Sql
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

-- | Performs raw query without arguments and returns the resulting rows.
queryRaw
    :: forall res env m .
       (WithDb env m, FromRow res)
    => Sql.Query
    -> m [res]
queryRaw q = withPool $ \conn -> Sql.query_ conn q
{-# INLINE queryRaw #-}

{- | Performs a query with arguments and returns the resulting rows with the
given parameters.
-}
query
    :: forall res args env m .
       (WithDb env m, ToRow args, FromRow res)
    => Sql.Query
    -> args
    -> m [res]
query q args = withPool $ \conn -> Sql.query conn q args
{-# INLINE query #-}

-- | Performs a query with named parameters and returns a list of rows.
queryNamed
    :: (WithError PgNamedError m, WithDb env m, FromRow res)
    => Sql.Query
    -> [NamedParam]
    -> m [res]
queryNamed q params =
    withPool (\conn -> runExceptT $ Sql.queryNamed conn q params) >>= liftError
{-# INLINE queryNamed #-}

{- | Performs a query with named parameters and custom 'RowParser' and returns a
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

-- | Executes a query without arguments that and return a number of affected rows.
executeRaw
    :: (WithDb env m)
    => Sql.Query
    -> m Int64
executeRaw q = withPool $ \conn -> Sql.execute_ conn q
{-# INLINE executeRaw #-}

-- | Executes a query without arguments that is not expected to return results.
executeRaw_
    :: (WithDb env m)
    => Sql.Query
    -> m ()
executeRaw_ = void . executeRaw
{-# INLINE executeRaw_ #-}

-- | Executes a query with parameters and return a number of affected rows.
execute
    :: forall args env m .
       (WithDb env m, ToRow args)
    => Sql.Query
    -> args
    -> m Int64
execute q args = withPool $ \conn -> Sql.execute conn q args
{-# INLINE execute #-}

-- | Executes a query with parameters that is not expected to return results.
execute_
    :: forall args env m .
       (WithDb env m, ToRow args)
    => Sql.Query
    -> args
    -> m ()
execute_ q args = void $ execute q args
{-# INLINE execute_ #-}

-- | Executes a multi-row query and returns a number of affected rows.
executeMany
    :: (WithDb env m, ToRow args)
    => Sql.Query
    -> [args]
    -> m Int64
executeMany q args = withPool $ \conn -> Sql.executeMany conn q args
{-# INLINE executeMany #-}

-- | Executes a multi-row query that is not expected to return results.
executeMany_
    :: (WithDb env m, ToRow args)
    => Sql.Query
    -> [args]
    -> m ()
executeMany_ q args = void $ executeMany q args
{-# INLINE executeMany_ #-}

{- | Executes a query with named parameters, returning the
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

{- | Executes a query with named parameters. Like 'executeNamed' but doesn't
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
-}
asSingleRowWith :: WithError err m => err -> m [a] -> m a
asSingleRowWith err action = throwOnNothingM err (viaNonEmpty head <$> action)
{-# INLINE asSingleRowWith #-}

-- | Helper to throw a given when defining a 'FromRow' instance
failParsingWith :: (Show err, Typeable err) => err -> Sql.RowParser a
failParsingWith err =
    Sql.fieldWith (\_ _ -> Sql.conversionError $ toNoSourceException err)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Perform action that needs database connection.
withPool :: WithDb env m => (Sql.Connection -> IO b) -> m b
withPool action = do
    pool <- grab @DbPool
    liftIO $ Pool.withResource pool action
{-# INLINE withPool #-}
