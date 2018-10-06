{-# LANGUAGE QuasiQuotes  #-}
module Spec.GraphQL.Parse (tests) where
---
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.Attoparsec.Text (parseOnly, Parser)
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
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
        testParse "full mutation" [graphql|mutation TestQ { amount posted }|] $
            DNExecutable (EDNOperation (ODNTyped MUTATION (Just "TestQ") Nothing Nothing
                (nempt [Field Nothing "amount" Nothing Nothing Nothing,
                    Field Nothing "posted" Nothing Nothing Nothing])
                    )) :| [],
        testParse "anon mutation" [graphql|mutation { amount posted }|] $
            DNExecutable (EDNOperation (ODNTyped MUTATION Nothing Nothing Nothing
                (nempt [sfield "amount",sfield "posted"])
                )) :| [],
        testParse "full subscription" [graphql|subscription TestQ { amount posted }|] $
            DNExecutable (EDNOperation (ODNTyped SUBSCRIPTION (Just "TestQ") Nothing Nothing
                (nempt [Field Nothing "amount" Nothing Nothing Nothing,
                    Field Nothing "posted" Nothing Nothing Nothing])
                    )) :| [],
        testParse "anon subscription" [graphql|subscription { amount posted }|] $
            DNExecutable (EDNOperation (ODNTyped SUBSCRIPTION Nothing Nothing Nothing
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
                )) :| [],
        testParseFail "newlines disallowed in normal string" "query ($a:Int = \"no line\nterm\") {email}",
        testParse "schema definition" [graphql|schema {
                                                query: TQuery
                                                mutation: TMutation
                                                subscription: TSubscription
                                        }|] $
                    nempt [DNTypeSystem (
                        TSDNRoots (nempt [
                            ROTDNDefinition QUERY "TQuery",
                            ROTDNDefinition MUTATION "TMutation",
                            ROTDNDefinition SUBSCRIPTION "TSubscription"])
                    )],
        testParse "directive definition" [graphql|directive
                        @big (
                            "name of the thing" name : String = 19 @defs
                            id : Int!
                            )
                        on QUERY|] $
                    nempt [DNTypeSystem (TSDNDirective (DDNDefinition "big"
                                (mnempt [
                                IVDN (Just "name of the thing") "name" (TNamed "String" False) (Just (VInt 19)) (mnempt [Directive "defs" Nothing]),
                                IVDN Nothing "id" (TNamed "Int" True) Nothing Nothing])
                            DL_QUERY))]
        ],

        testParse "scalar type definition" [graphql|
                    "name of horse" scalar name @defs
                    scalar Taco|] $
                    nempt [
                        DNTypeSystem (TSDNType TDN),
                        DNTypeSystem (TSDNType TDN)
                        ],


        testGroup "Directive Locations" [
            testDirectiveLocation "QUERY" DL_QUERY,
            testDirectiveLocation "MUTATION" DL_MUTATION,
            testDirectiveLocation "SUBSCRIPTION" DL_SUBSCRIPTION,
            testDirectiveLocation "FIELD" DL_FIELD,
            testDirectiveLocation "FRAGMENT_DEFINITION" DL_FRAGMENT_DEFINITION,
            testDirectiveLocation "FRAGMENT_SPREAD" DL_FRAGMENT_SPREAD,
            testDirectiveLocation "INLINE_FRAGMENT" DL_INLINE_FRAGMENT,
            testDirectiveLocation "SCHEMA" DL_SCHEMA,
            testDirectiveLocation "SCALAR" DL_SCALAR,
            testDirectiveLocation "OBJECT" DL_OBJECT,
            testDirectiveLocation "FIELD_DEFINITION" DL_FIELD_DEFINITION,
            testDirectiveLocation "ARGUMENT_DEFINITION" DL_ARGUMENT_DEFINITION,
            testDirectiveLocation "INTERFACE" DL_INTERFACE,
            testDirectiveLocation "UNION" DL_UNION,
            testDirectiveLocation "ENUM" DL_ENUM,
            testDirectiveLocation "ENUM_VALUE" DL_ENUM_VALUE,
            testDirectiveLocation "INPUT_OBJECT" DL_INPUT_OBJECT,
            testDirectiveLocation "INPUT_FIELD_DEFINITION" DL_INPUT_FIELD_DEFINITION
        ],

        testGroup "Values" [
            testValue "int" "7" (VInt 7),
            testValue "int from" "7.000" (VInt 7),
            testValue "float" "7.034" (VFloat 7.034),
            testValue "float e" "-112.3e-4" (VFloat (-112.3e-4)),
            testValue "string" "\"this is string\"" (VString "this is string"),
            testValue "string with spaces" "\"  this is string  \"" (VString "  this is string  "),
            testValue "string with escapes" "\"a \\t b\\r \\\\ s\\\"t \\f\\/\\n\\b\"" (VString "a \t b\r \\ s\"t \f/\n\b"),
            testValue "string with unicode escapes ""\" \\uabcd \\u12345\"" (VString (" \xABCD \x1234" <> "5")),
            testValue "block quote string indent" "\"\"\" \n \n this is\n   indented\n somewhat\\\"\"\"\n\n \n   \"\"\"" (VString ("this is\n  indented\nsomewhat\"\"\"")),
            testValue "bool - true" "true" (VBool True),
            testValue "bool - false" "false" (VBool False),
            testValue "null" "null" VNull,
            testValue "enum" "CLOSED" (VEnum "CLOSED"),
            testValue "list" "[7 1.3 \"seven\"]" (VList [VInt 7, VFloat 1.3, VString "seven"]),
            testValue "empty list" "[]" (VList []),
            testValue "object" "{ birthday : \"happy\", age: 3}" (VObject [("birthday", VString "happy"), ("age", VInt 3)]),
            testValue "variable" "$input" (VVariable "input")
            ]
    ]

testDirectiveLocation :: Text -> DirectiveLocation -> TestTree
testDirectiveLocation s v = testCase (unpack s) $ parseOnly document (
        "directive @a on " <> s
        ) @?= Right (nempt [DNTypeSystem (TSDNDirective (DDNDefinition "a" Nothing v))])

testParseFail :: [Char] -> Text -> TestTree
testParseFail name invalid = testCase name $
    isLeft (parseOnly document invalid) @?= True

testParse :: [Char] -> DocumentNode -> DocumentNode -> TestTree
testParse name actual expected = testCase name $ actual @?= expected

testValue :: [Char] -> Text -> Value -> TestTree
testValue name lit expected = testCase name $
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

