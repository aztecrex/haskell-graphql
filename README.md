# GraphQL Support Library

[![Build Status](https://travis-ci.org/aztecrex/haskell-graphql.svg?branch=master)](https://travis-ci.org/aztecrex/haskell-graphql)

## Status

The parser can handle queries and mutations. Current work is on subscriptions.

## Design

This is meant to be a straightforward GQL implementation. It doesn't encode a
schema into a Servent-ish type like [this library](https://github.com/haskell-graphql/graphql-api).
That one looks really neat so you should try it.

## Scope

The scope of this project is to provide enough support so that it is fairly easy to implement
a GQL server or client.

### Parse

The parsing functions will be capable of mapping arbitrary GQL document to the AST.

### Validate

The validation function(s) will be able to determine a valid ST.

### Execute

The execution function(s) will allow a programmer to configure resolvers and run executable
definitions with them.

### Issue

The issue functions will allow a programmer to form and send GQL requests to a service and interpret
the results easily.

### Unparse

The unparse function(s) will be capable of converting an ST into a form that can be consumed by
common tools such as [Graphiql](https://github.com/graphql/graphiql).


## Build

The project uses the [Haskell Stack](https://docs.haskellstack.org/en/stable/README/). If you don't
have it, use the instructions on the site. Once you have `stack` installed:
- `stack build` to build the project
- `stack test` to run the tests

