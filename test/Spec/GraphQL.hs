module Spec.GraphQL (tests) where
---
import Test.Tasty (TestTree, testGroup)

import qualified Spec.GraphQL.Syntax as Syntax (tests)

tests :: TestTree
tests = testGroup "GraphQL" [
    Syntax.tests
    ]
