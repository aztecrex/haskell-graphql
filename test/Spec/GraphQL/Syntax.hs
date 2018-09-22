{-# LANGUAGE QuasiQuotes #-}
module Spec.GraphQL.Syntax (tests) where
---

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.List.NonEmpty (NonEmpty ((:|)))

import Language.GraphQL.TH (graphql)
import Language.GraphQL.Syntax

tests :: TestTree
tests = testGroup ("Syntax") [
    testCase "simple" $ do
        let
        -- when
            parsed = [graphql|{ amount }|]

        -- then
            expected = Document $
                (ExecutableDefinition
                    (OpDefExecutableDefinition (
                        SelSetOperationDefinition
                            (SelectionSet [
                                Field Nothing "amount" Nothing Nothing Nothing
                            ])
                        )
                    )
                ) :| []
        parsed @?= expected
    ]

