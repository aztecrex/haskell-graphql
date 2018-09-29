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
    testGroup "Kitchen Sink" [
        testParse "full query" [graphql|query TestQ { amount posted }|] $
            DNExecutable (EDNOperation (ODNTyped QUERY (Just "TestQ") Nothing Nothing
                [Field Nothing "amount" Nothing Nothing Nothing,
                 Field Nothing "posted" Nothing Nothing Nothing]
                 )) :| [],
        testParse "anon query" [graphql|query { amount posted }|] $
            DNExecutable (EDNOperation (ODNTyped QUERY Nothing Nothing Nothing
                [Field Nothing "amount" Nothing Nothing Nothing,
                Field Nothing "posted" Nothing Nothing Nothing]
                )) :| [],
        testParse "shorthand query" [graphql|{ amount posted }|] $
            DNExecutable (EDNOperation (ODNSelectionSet
                [Field Nothing "amount" Nothing Nothing Nothing,
                Field Nothing "posted" Nothing Nothing Nothing]
                )) :| [],
        testParse "variables" [graphql|query (
                        $a: Int!
                        $b: Float = 7
                        $c: [[Bool!]!]
                    ) {}|] $
            DNExecutable (EDNOperation (
                        ODNTyped QUERY Nothing (
                            Just (
                                VariableDefinition "a" (TNamed "Int" True) Nothing
                            :| [
                                VariableDefinition "b" (TNamed "Float" False) (Just (VInt 7)),
                                VariableDefinition "c" (TList (TList (TNamed "Bool" True) True) False) Nothing
                                ]
                    )) Nothing []
                )) :| []
            ]
    ]

testParse :: [Char] -> DocumentNode -> DocumentNode -> TestTree
testParse name actual expected = testCase name $ actual @?= expected

-- Field (Maybe Text) Text (Maybe Arguments) (Maybe Directives) (Maybe SelectionSet)
