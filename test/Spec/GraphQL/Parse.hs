{-# LANGUAGE QuasiQuotes  #-}
module Spec.GraphQL.Parse where
---
import Language.GraphQL.Syntax
import Language.GraphQL.TH

x :: Document
x = [graphql|{ amount }|]


