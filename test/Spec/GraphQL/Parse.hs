{-# LANGUAGE QuasiQuotes  #-}
module Spec.GraphQL.Parse (tests) where
---
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.List.NonEmpty (NonEmpty ((:|)))
import Language.GraphQL.Syntax
import Language.GraphQL.TH (graphql)

tests :: TestTree
tests = testGroup "Parse" [

    testCase "full query syntax" $ do
        let
        -- when
            parsed = [graphql|query TestQ { amount posted }|]

        -- then
            expected = Document $
                (ExecutableDefinition
                    (OpDefExecutableDefinition
                        (OpTypeOperationDefinition QUERY (Just "TestQ") Nothing Nothing
                            (SelectionSet [
                                Field Nothing "amount" Nothing Nothing Nothing,
                                Field Nothing "posted" Nothing Nothing Nothing
                            ])
                        )
                    )
                ) :| []
        parsed @?= expected,

    testCase "unnamed query syntax" $ do
        let
        -- when
            parsed = [graphql|query { amount posted }|]

        -- then
            expected = Document $
                (ExecutableDefinition
                    (OpDefExecutableDefinition
                        (OpTypeOperationDefinition QUERY Nothing Nothing Nothing
                            (SelectionSet [
                                Field Nothing "amount" Nothing Nothing Nothing,
                                Field Nothing "posted" Nothing Nothing Nothing
                            ])
                        )
                    )
                ) :| []
        parsed @?= expected,

    testCase "shorthand query syntax" $ do
        let
        -- when
            parsed = [graphql|{ amount posted }|]

        -- then
            expected = Document $
                (ExecutableDefinition
                    (OpDefExecutableDefinition
                        (SelSetOperationDefinition
                            (SelectionSet [
                                Field Nothing "amount" Nothing Nothing Nothing,
                                Field Nothing "posted" Nothing Nothing Nothing
                            ])
                        )
                    )
                ) :| []
        parsed @?= expected,

    testCase "ignore commas" $
            [graphql|{amount posted customer}|] @?= [graphql|{amount, posted customer}|]


    ]
