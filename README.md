# cake-slayer

![logo](https://user-images.githubusercontent.com/4276606/67370235-fc5cbb00-f582-11e9-9f45-09bf96ee6d0c.png)

[![GitHub CI](https://github.com/kowainik/cake-slayer/workflows/CI/badge.svg)](https://github.com/kowainik/cake-slayer/actions)
[![Hackage](https://img.shields.io/hackage/v/cake-slayer.svg?logo=haskell)](https://hackage.haskell.org/package/cake-slayer)
[![Stackage Lts](http://stackage.org/package/cake-slayer/badge/lts)](http://stackage.org/lts/package/cake-slayer)
[![Stackage Nightly](http://stackage.org/package/cake-slayer/badge/nightly)](http://stackage.org/nightly/package/cake-slayer)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)

`cake-slayer` (do not confuse with `cakes-layer`) is a modern and
batteries-included framework for creating backend in Haskell for
web-applications. It allows you to scaffold working and extensible project in
minutes.

## Overview

The main goal of `cake-slayer` is to provide a backbone for your Haskell
backend. Unlike many other Haskell libraries, `cake-slayer` doesn't try to be as
abstract as possible. It includes best-practices and makes some architecture
decisions for you. On the one hand, your application should satisfy the
requirements for using this framework. On the other hand, it takes care of a lot
of stuff for you, so you don't need to worry about everyday problems.

A typical backend does the following:

* Communicates with the frontend (web, mobile) via some wire format (JSON,
  Protocol Buffers, etc.)
* Talks to the database
* Authenticates and authorizes users
* Has some background jobs

`cake-slayer` works exceptionally well if your application uses:

1. PostgreSQL as database.
2. Elm on the frontend.

The following Haskell packages have been chosen to provide the necessary
functional:

* [`postgresql-simple`](https://hackage.haskell.org/package/postgresql-simple),
  [`postgresql-simple-named`](https://hackage.haskell.org/package/postgresql-simple-named),
  [`postgresql-simple-migration`](https://hackage.haskell.org/package/postgresql-simple-migration)
  + For connecting and talking to the PostgreSQL database
* [`servant`](http://hackage.haskell.org/package/servant),
  [`servant-swagger`](http://hackage.haskell.org/package/servant-swagger),
  [`swagger2`](http://hackage.haskell.org/package/swagger2)
  + A family of libraries for defining and documenting Rest API using type-level eDSL
* [`elm-street`](http://hackage.haskell.org/package/elm-street)
  + The bridge between Elm and Haskell — generating Elm data types, JSON
    encoders and decoders from Haskell types automatically
* [`jwt`](http://hackage.haskell.org/package/jwt)
  + User authentication via JWT
* [`bcrypt`](http://hackage.haskell.org/package/bcrypt)
  + Secure password hashing
* [`ekg`](http://hackage.haskell.org/package/ekg),
  [`prometheus-client`](https://hackage.haskell.org/package/prometheus-client)
  + Application performance monitoring.
  > **NOTE:** Using `prometheus-client` is optional

Besides `cake-slayer` encourages (but doesn't force) to use the following libraries:

* [`relude`](https://hackage.haskell.org/package/relude)
  + Alternative standard library
* [`co-log`](https://hackage.haskell.org/package/co-log)
  + [Composable contravariant comonadic logging library](https://kowainik.github.io/posts/2018-09-25-co-log)

## How to use?

The `cake-slayer` framework contains implementations of most common and useful
functions. But at the same time, it provides enough flexibility to specify
application-specific parts. To integrate `cake-slayer` smoothly into your
project, you should perform the following steps:

1. Define a type of errors your application can throw. See `CakeSlayer.Error`
   module for details.
2. Define a monad for your application by specializing the `App` monad from the
   `CakeSlayer.Monad` module.
3. Derive or implement all necessary instances for your application monad.
   `cake-slayer` provides `MonadJwt` and `MonadTimed` effects with sensible
   default implementations for your convenience.

And you're good to go!
