{-# LANGUAGE QuasiQuotes  #-}
module Spec.GraphQL.Parse (tests) where
---
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.Attoparsec.Text (parseOnly)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid ((<>))
import Data.Text (Text)
import Language.GraphQL.Parse
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
                        $b2: Float = 7.03
                        $z: String! = "very carefully"
                        $c: [[Bool!]!]
                    ) {}|] $
            DNExecutable (EDNOperation (
                        ODNTyped QUERY Nothing (
                            Just (
                                VariableDefinition "a" (TNamed "Int" True) Nothing
                            :| [
                                VariableDefinition "b" (TNamed "Float" False) (Just (VInt 7)),
                                VariableDefinition "b2" (TNamed "Float" False) (Just (VFloat 7.03)),
                                VariableDefinition "z"  (TNamed "String" True) (Just (VString "very carefully")),
                                VariableDefinition "c" (TList (TList (TNamed "Bool" True) True) False) Nothing
                                ]
                    )) Nothing []
                )) :| []
            ],
        testGroup "Values" [
            testLiteral "int" "7" (VInt 7),
            testLiteral "int from" "7.000" (VInt 7),
            testLiteral "float" "7.034" (VFloat 7.034),
            testLiteral "float e" "-112.3e-4" (VFloat (-112.3e-4)),
            testLiteral "string" "\"this is string\"" (VString "this is string"),
            testLiteral "string with spaces" "\"  this is string  \"" (VString "  this is string  "),
            testLiteral "bool - true" "true" (VBool True),
            testLiteral "bool - false" "false" (VBool False),
            testLiteral "null" "null" VNull,
            testLiteral "enum" "CLOSED" (VEnum "CLOSED"),
            testLiteral "list" "[7 1.3 \"seven\"]" (VList [VInt 7, VFloat 1.3, VString "seven"]),
            testLiteral "empty list" "[]" (VList []),
            testLiteral "object" "{ birthday : \"happy\", age: 3}" (VObject [("birthday", VString "happy"), ("age", VInt 3)])
            ]
    ]

testParse :: [Char] -> DocumentNode -> DocumentNode -> TestTree
testParse name actual expected = testCase name $ actual @?= expected

testLiteral :: [Char] -> Text -> Value -> TestTree
testLiteral name lit expected = testCase name $
                (parseOnly document ("query ($a:Int = " <> lit <> ") {}")) @?=
                (
                    Right (
                        (DNExecutable (
                            EDNOperation(
                                ODNTyped QUERY Nothing (
                                    Just (
                                        (VariableDefinition "a" (TNamed "Int" False) (Just expected)) :| []
                                    )
                                ) Nothing []
                            )
                        ) :| [])
                    )
                )

-- Field (Maybe Text) Text (Maybe Arguments) (Maybe Directives) (Maybe SelectionSet)
