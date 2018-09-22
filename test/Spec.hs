{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE QuasiQuotes  #-}

import Test.Tasty (TestTree, testGroup, defaultMain)
import qualified Spec.GraphQL as GraphQL (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "all" [
    GraphQL.tests
    ]
