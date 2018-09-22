module Spec.GraphQL (tests) where
---
import Test.Tasty (TestTree, testGroup)

import qualified Spec.GraphQL.Parse as Parse (tests)
import qualified Spec.GraphQL.Syntax as Syntax (tests)

tests :: TestTree
tests = testGroup "GraphQL" [
    Syntax.tests,
    Parse.tests

    ]
