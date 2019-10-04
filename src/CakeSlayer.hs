{- | The main module that reexports all functionality. You can import only
"CakeSlayer" module to have everything that @cake-slayer@ is capable of.

Alternatively, you are free to import each module individually for more
standalone imports.

-}
module CakeSlayer
       ( module CakeSlayer.Db
         -- $db
       , module CakeSlayer.Error
         -- $error
       , module CakeSlayer.Has
         -- $has
       , module CakeSlayer.Jwt
         -- $jwt
       , module CakeSlayer.Measure
         -- $measure
       , module CakeSlayer.Monad
         -- $monad
       , module CakeSlayer.Password
         -- $password
       , module CakeSlayer.Random
         -- $random
       , module CakeSlayer.Servant
         -- $servant
       , module CakeSlayer.SqlArray
         -- $sqlarray
       , module CakeSlayer.Swagger
         -- $swagger
       , module CakeSlayer.Time
         -- $time
       ) where

import CakeSlayer.Db
import CakeSlayer.Error
import CakeSlayer.Has
import CakeSlayer.Jwt
import CakeSlayer.Measure
import CakeSlayer.Monad
import CakeSlayer.Password
import CakeSlayer.Random
import CakeSlayer.Servant
import CakeSlayer.SqlArray
import CakeSlayer.Swagger
import CakeSlayer.Time

{- $db
"CakeSlayer.Db" contains useful helpers and wrapper-functions around
@postgresql-simple@, @postgresql-simple-named@ and @postgresql-simple-migration@
libraries. Also introduces abstractions and functions for 'Data.Pool.Pool'.
-}

{- $error
"CakeSlayer.Error" contains types ans functions for the app error customization.
-}

{- $has
"CakeSlayer.Has" contains functions and type clases to work with the environment
data type.
-}

{- $jwt
"CakeSlayer.Jwt" provides convenient wrappers around JWT provided by the @jwt@ library.
-}

{- $measure
"CakeSlayer.Measure" contains useful type classes for dealing with the
application metrics.
-}

{- $monad
"CakeSlayer.Monad" contains the main monad around which your application
should be build.
-}

{- $password
"CakeSlayer.Password" contains data types and functions to work with passwords
in your application.
-}

{- $random
"CakeSlayer.Random" contains functions to work with randomness.
-}

{- $servant
"CakeSlayer.Servant" contains useful type alias to work with @servant@ and
generic servant approach.
-}

{- $sqlarray
"CakeSlayer.SqlArray" contains the wrapper around lists to work with DB arrays.
-}

{- $swagger
"CakeSlayer.Swagger" contains helper functions to create @swagger@ instances.
-}

{- $time
"CakeSlayer.Time" contains helper functions to work with time.
-}
