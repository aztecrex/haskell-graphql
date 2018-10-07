module Spec.Language.GraphQL (tests) where
---
import Test.Tasty (TestTree, testGroup)

import qualified Spec.Language.GraphQL.Parse as Parse (tests)
import qualified Spec.Language.GraphQL.Syntax as Syntax (tests)

tests :: TestTree
tests = testGroup "GraphQL" [
    Syntax.tests,
    Parse.tests

    ]
