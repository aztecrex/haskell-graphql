module Spec.Language (tests) where
---
import Test.Tasty (TestTree, testGroup)

import qualified Spec.Language.GraphQL as GraphQL (tests)

tests :: TestTree
tests = testGroup "Language" [
    GraphQL.tests
    ]
