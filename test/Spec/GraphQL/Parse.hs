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
                (nempt [Field Nothing "amount" Nothing Nothing Nothing,
                 Field Nothing "posted" Nothing Nothing Nothing])
                 )) :| [],
        testParse "anon query" [graphql|query { amount posted }|] $
            DNExecutable (EDNOperation (ODNTyped QUERY Nothing Nothing Nothing
                (nempt [sfield "amount",sfield "posted"])
                )) :| [],
        testParse "shorthand query" [graphql|{ amount posted }|] $
            DNExecutable (EDNOperation (ODNSelectionSet
                (nempt [sfield "amount",sfield "posted"])
                )) :| [],
        testParse "sub-select" [graphql|{ amount posted {timestamp, by} }|] $
            DNExecutable (EDNOperation (ODNSelectionSet
                (nempt [
                    sfield "amount",
                    Field Nothing "posted" Nothing Nothing (mnempt [
                        sfield "timestamp",
                        sfield "by"
                        ])])
                )) :| [],
            testParse "alias" [graphql|{ sales: amount }|] $
            DNExecutable (EDNOperation (ODNSelectionSet
                (nempt [Field (Just "sales") "amount" Nothing Nothing Nothing])
                )) :| [],
            testParse "variables" [graphql|query (
                        $a: Int!
                        $b: Float = 7
                        $b2: Float = 7.03
                        $z: String! = "very carefully"
                        $c: [[Bool!]!]
                    ) {user}|] $
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
                    )) Nothing (nempt [sfield "user"])
                )) :| [],
        testParse "arguments" [graphql|{user(id:5 name: "hi")}|] $
            DNExecutable (EDNOperation (ODNSelectionSet (nempt [
                Field Nothing "user" (Just (Argument "id" (VInt 5) :| [Argument "name" (VString "hi")])) Nothing Nothing
            ])
            )) :| [],
        testParse "directives - query" [graphql|query @purple (id: 9) @rain {amount}|] $
            DNExecutable (EDNOperation (
                ODNTyped QUERY Nothing Nothing (Just (
                    Directive "purple" (Just (Argument "id" (VInt 9) :| [] ))
                    :| [
                    Directive "rain" Nothing
                    ])) (nempt [sfield "amount"])
            )) :| [],
        testParse "directives - field" [graphql|{user @final}|] $
            DNExecutable (EDNOperation (ODNSelectionSet (nempt [
                Field Nothing "user" Nothing (Just (Directive "final" Nothing :| [])) Nothing
            ])
            )) :| [],
        testParse "directives - fragment" [graphql|fragment Profile on User @fast(cool: "beans") {email}|] $
            DNExecutable (EDNFragment (
                    FragmentDefinition "Profile" "User"
                                (Just (Directive "fast"
                                    (Just (Argument "cool" (VString "beans") :| [] ))
                                :| []))
                                (nempt [sfield "email"])
                )) :| [],
        testParse "directives - fragment spread" [graphql|{  ... Door @hollow }|] $
            DNExecutable (EDNOperation (ODNSelectionSet
                (nempt [FragmentSpread "Door" (mnempt [Directive "hollow" Nothing])])
                )) :| [],
            testParse "fragments" [graphql|fragment Profile on User {email name}|] $
            DNExecutable (EDNFragment (
                    FragmentDefinition "Profile" "User" Nothing (nempt [
                                sfield "email",
                                sfield "name"])
                )) :| [],
            testParse "multiple definitions" [graphql|
                fragment Profile on User {email name}
                {me} # current user
                # this doesn't count
                fragment Variation on Recipe {flavor}
                # several things|] $
            DNExecutable (EDNFragment (
                    FragmentDefinition "Profile" "User" Nothing (nempt [
                                sfield "email",
                                sfield "name"])
                )) :| [
            DNExecutable (EDNOperation (ODNSelectionSet
                    (nempt [sfield "me"])
                )),
            DNExecutable (EDNFragment (
                FragmentDefinition "Variation" "Recipe" Nothing (nempt [sfield "flavor"])
                ))
                ],
        testParse "fragment spread" [graphql|{ amount posted ... Door }|] $
            DNExecutable (EDNOperation (ODNSelectionSet
                (nempt [sfield "amount",sfield "posted",
                        FragmentSpread "Door" Nothing])
                )) :| [],
        testParse "inline fragment" [graphql|{ amount posted ... on User {email} ... {address} }|] $
            DNExecutable (EDNOperation (ODNSelectionSet
                (nempt [sfield "amount",sfield "posted",
                        InlineFragment (Just "User") Nothing (nempt [sfield "email"] ),
                        InlineFragment Nothing Nothing (nempt [sfield "address"] )
                    ])
                )) :| []
                ],

                -- InlineFragment (Maybe Text) (Maybe Directives) SelectionSet

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
                (parseOnly document ("query ($a:Int = " <> lit <> ") {q}")) @?=
                (
                    Right (
                        (DNExecutable (
                            EDNOperation(
                                ODNTyped QUERY Nothing (
                                    Just (
                                        (VariableDefinition "a" (TNamed "Int" False) (Just expected)) :| []
                                    )
                                ) Nothing (nempt ([sfield "q"]))
                            )
                        ) :| [])
                    )
                )

sfield :: Text -> Selection
sfield n = Field Nothing n Nothing Nothing Nothing

nempt :: [a] -> NonEmpty a
nempt [] = undefined
nempt (a : as) = a :| as

mnempt :: [a] -> Maybe (NonEmpty a)
mnempt [] = Nothing
mnempt as = Just (nempt as)

