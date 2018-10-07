{-# LANGUAGE QuasiQuotes  #-}
module Spec.Language.GraphQL.Parse (tests) where
---
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.Attoparsec.Text (parseOnly)
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
                        DNTypeSystem (TSDNType (TDNScalar (Just "name of horse") "name"
                                (mnempt [Directive "defs" Nothing]))),
                        DNTypeSystem (TSDNType (TDNScalar Nothing "Taco" Nothing))
                        ],

        testParse "enum type definition" [graphql|
                    "kind of horse" enum Kind @defs {
                            "color is brown" BROWN @valuable
                            SPOTTED
                        }
                        enum Filling {ALPASTOR CHORIZO CABEZA}
                        enum Huh|] $
                    nempt [
                        DNTypeSystem ( TSDNType ( TDNEnum
                            (Just "kind of horse")
                            "Kind"
                            (mnempt [Directive "defs" Nothing])
                            (mnempt [
                                EnumValueDef (Just "color is brown") "BROWN" (mnempt [
                                    Directive "valuable" Nothing
                                ]),
                                EnumValueDef Nothing "SPOTTED" Nothing
                            ])
                        )),
                        DNTypeSystem ( TSDNType ( TDNEnum
                            Nothing
                            "Filling"
                            Nothing
                            (mnempt [
                                EnumValueDef Nothing "ALPASTOR" Nothing,
                                EnumValueDef Nothing "CHORIZO" Nothing,
                                EnumValueDef Nothing "CABEZA" Nothing
                            ])
                        )),
                        DNTypeSystem ( TSDNType ( TDNEnum
                            Nothing
                            "Huh"
                            Nothing
                            Nothing
                        ))
                        ],

        testParse "object type definition" [graphql|
                    "horse props" type Props implements & Animal & Asset @defs {
                        "regular octane" SPf ("fuel factor" f : String = PEARL @over) : Int! @corner
                        dsp : String
                        }
                    "horse props" type Props implements Animal & Asset @defs {
                        "regular octane" SPf ("fuel factor" f : String = PEARL @over) : Int! @corner
                        dsp : String
                    }
                    type Taco {filling : Filling! shell : Material}
                    type WhatGoodIsThis
                    |] $
                    nempt [
                        DNTypeSystem (TSDNType (TDNObject
                            (Just "horse props")
                            "Props"
                            (mnempt ["Animal", "Asset"])
                            (mnempt [Directive "defs" Nothing])
                            (mnempt [
                                FieldDefinition
                                    (Just "regular octane")
                                    "SPf"
                                    (mnempt [
                                        IVDN (Just "fuel factor") "f" (TNamed "String" False) (Just (VEnum "PEARL")) (mnempt [Directive "over" Nothing])
                                    ])
                                    (TNamed "Int" True)
                                    (mnempt [Directive "corner" Nothing]),
                                FieldDefinition
                                    Nothing
                                    "dsp"
                                    Nothing
                                    (TNamed "String" False)
                                    Nothing
                            ])
                        )),
                        DNTypeSystem (TSDNType (TDNObject
                            (Just "horse props")
                            "Props"
                            (mnempt ["Animal", "Asset"])
                            (mnempt [Directive "defs" Nothing])
                            (mnempt [
                                FieldDefinition
                                    (Just "regular octane")
                                    "SPf"
                                    (mnempt [
                                        IVDN (Just "fuel factor") "f" (TNamed "String" False) (Just (VEnum "PEARL")) (mnempt [Directive "over" Nothing])
                                    ])
                                    (TNamed "Int" True)
                                    (mnempt [Directive "corner" Nothing]),
                                FieldDefinition
                                    Nothing
                                    "dsp"
                                    Nothing
                                    (TNamed "String" False)
                                    Nothing
                            ])
                        )),
                        DNTypeSystem (TSDNType (TDNObject
                            Nothing
                            "Taco"
                            Nothing
                            Nothing
                            (mnempt [
                                FieldDefinition Nothing "filling" Nothing (TNamed "Filling" True) Nothing,
                                FieldDefinition Nothing "shell" Nothing (TNamed "Material" False) Nothing
                            ])
                        )),
                        DNTypeSystem (TSDNType (TDNObject
                            Nothing "WhatGoodIsThis" Nothing Nothing Nothing
                        ))
                        ],

        testParse "interface type definition" [graphql|
                    "animal props" interface Animal @scorp {
                            "withholdings before taxes" dollarinos ("scuba diver" nominal : String = never @under) : Int! @paced
                            light : Bulb
                        }
                    interface Bulb {filament : Material power : Watts!}
                    interface ReallyWhatGoodIsThis
                    |] $
                    nempt [
                        DNTypeSystem (TSDNType (TDNInterface
                            (Just "animal props")
                            "Animal"
                            (mnempt [Directive "scorp" Nothing])
                            (mnempt [
                                FieldDefinition
                                    (Just "withholdings before taxes")
                                    "dollarinos"
                                    (mnempt [
                                        IVDN (Just "scuba diver") "nominal" (TNamed "String" False) (Just (VEnum "never")) (mnempt [Directive "under" Nothing])
                                    ])
                                    (TNamed "Int" True)
                                    (mnempt [Directive "paced" Nothing]),
                                FieldDefinition
                                    Nothing
                                    "light"
                                    Nothing
                                    (TNamed "Bulb" False)
                                    Nothing
                            ])
                        )),
                        DNTypeSystem (TSDNType (TDNInterface
                            Nothing
                            "Bulb"
                            Nothing
                            (mnempt [
                                FieldDefinition
                                    Nothing
                                    "filament"
                                    Nothing
                                    (TNamed "Material" False)
                                    Nothing,
                                FieldDefinition
                                    Nothing
                                    "power"
                                    Nothing
                                    (TNamed "Watts" True)
                                    Nothing
                            ])
                        )),
                        DNTypeSystem (TSDNType (TDNInterface
                            Nothing
                            "ReallyWhatGoodIsThis"
                            Nothing
                            Nothing
                        ))
                        ],

        testParse "union type definition" [graphql|
                    "whatever we want to return" union Thing @scorp   Animal | Taco | Airplane
                    "whatever we want to return" union Thing @scorp | Animal | Taco | Airplane
                    union Care   Hospital | Delivery
                    union Care | Hospital | Delivery
                    union StillMakesNoSense
                    |] $
                    nempt [
                        DNTypeSystem (TSDNType (TDNUnion
                            (Just "whatever we want to return")
                            "Thing"
                            (mnempt [Directive "scorp" Nothing])
                            (mnempt ["Animal", "Taco", "Airplane"])
                        )),
                        DNTypeSystem (TSDNType (TDNUnion
                            (Just "whatever we want to return")
                            "Thing"
                            (mnempt [Directive "scorp" Nothing])
                            (mnempt ["Animal", "Taco", "Airplane"])
                        )),
                        DNTypeSystem (TSDNType (TDNUnion
                            Nothing
                            "Care"
                            Nothing
                            (mnempt ["Hospital", "Delivery"])
                        )),
                        DNTypeSystem (TSDNType (TDNUnion
                            Nothing
                            "Care"
                            Nothing
                            (mnempt ["Hospital", "Delivery"])
                        )),
                        DNTypeSystem (TSDNType (TDNUnion
                            Nothing
                            "StillMakesNoSense"
                            Nothing
                            Nothing
                        ))
                        ],

        testParse "input object type definition" [graphql|
                    "household items" input Parms @defs {
                            "a ladder 6 feet" normalized : Structure! = true @corner
                            never : String
                        }
                    input Recipe { feeds : Int! }
                    input YetAnotherEmptyDefinition
                    |] $
                    nempt [
                        DNTypeSystem (TSDNType (TDNInput
                            (Just "household items")
                            "Parms"
                            (mnempt [Directive "defs" Nothing])
                            (mnempt [
                                IVDN (Just "a ladder 6 feet") "normalized" (TNamed "Structure" True) (Just (VBool True)) (mnempt [Directive "corner" Nothing]),
                                IVDN Nothing "never" (TNamed "String" False) Nothing Nothing
                            ])
                        )),
                        DNTypeSystem (TSDNType (TDNInput
                            Nothing
                            "Recipe"
                            Nothing
                            (mnempt [
                                IVDN Nothing "feeds" (TNamed "Int" True) Nothing Nothing
                            ])
                        )),
                        DNTypeSystem (TSDNType (TDNInput
                            Nothing
                            "YetAnotherEmptyDefinition"
                            Nothing
                            Nothing
                        ))
                        ],

        testParse "schema extension" [graphql|
                extend schema @dir1 @dir2 { mutation: Mu query: Qu subscription: Su}
                extend schema { query: Q subscription: S }
                extend schema @abc @def
                |] $
                nempt [
                    DNTypeSystemExtension (TSENSchema (SchemaExtendRoots
                        (mnempt [Directive "dir1" Nothing, Directive "dir2" Nothing])
                        (nempt [
                            ROTDNDefinition MUTATION "Mu",
                            ROTDNDefinition QUERY "Qu",
                            ROTDNDefinition SUBSCRIPTION "Su"
                            ])
                    )),
                    DNTypeSystemExtension (TSENSchema (SchemaExtendRoots
                        Nothing
                        (nempt [
                            ROTDNDefinition QUERY "Q",
                            ROTDNDefinition SUBSCRIPTION "S"
                        ])
                    )),
                    DNTypeSystemExtension (TSENSchema (SchemaExtendDirectives
                        (nempt [Directive "abc" Nothing, Directive "def" Nothing])
                    ))
                ],

        testParse "scalar extension" [graphql|extend scalar Store @dirA @dirC|] $
                nempt [
                    DNTypeSystemExtension (TSENType (TENScalar
                        "Store"
                        (nempt [Directive "dirA" Nothing, Directive "dirC" Nothing])
                    ))
                ],

        testParse "object extension" [graphql|
                extend type Obj implements & Truck & Airplane @spin @jump {egg : String}
                extend type Obj implements Truck & Airplane @spin @jump {egg : String}
                extend type Obj @spin @jump {egg : String}
                extend type Obj implements Truck & Airplane {egg : String}
                extend type Obj {egg: String}
                extend type Obj implements & Truck & Airplane @spin @jump
                extend type Obj implements Truck & Airplane @spin @jump
                extend type Obj @spin @jump
                extend type Obj implements & Truck & Airplane
                extend type Obj implements Truck & Airplane
                |] $
                nempt [
                    DNTypeSystemExtension (TSENType (TENObjectF
                        "Obj"
                        (mnempt ["Truck", "Airplane"])
                        (mnempt [Directive "spin" Nothing, Directive "jump" Nothing])
                        (nempt [
                            FieldDefinition Nothing "egg" Nothing (TNamed "String" False) Nothing
                        ])
                    )),
                    DNTypeSystemExtension (TSENType (TENObjectF
                        "Obj"
                        (mnempt ["Truck", "Airplane"])
                        (mnempt [Directive "spin" Nothing, Directive "jump" Nothing])
                        (nempt [
                            FieldDefinition Nothing "egg" Nothing (TNamed "String" False) Nothing
                        ])
                    )),
                    DNTypeSystemExtension (TSENType (TENObjectF
                        "Obj"
                        Nothing
                        (mnempt [Directive "spin" Nothing, Directive "jump" Nothing])
                        (nempt [
                            FieldDefinition Nothing "egg" Nothing (TNamed "String" False) Nothing
                        ])
                    )),
                    DNTypeSystemExtension (TSENType (TENObjectF
                        "Obj"
                        (mnempt ["Truck", "Airplane"])
                        Nothing
                        (nempt [
                            FieldDefinition Nothing "egg" Nothing (TNamed "String" False) Nothing
                        ])
                    )),
                    DNTypeSystemExtension (TSENType (TENObjectF
                        "Obj"
                        Nothing
                        Nothing
                        (nempt [
                            FieldDefinition Nothing "egg" Nothing (TNamed "String" False) Nothing
                        ])
                    )),
                    DNTypeSystemExtension (TSENType (TENObjectD
                        "Obj"
                        (mnempt ["Truck", "Airplane"])
                        (nempt [Directive "spin" Nothing, Directive "jump" Nothing])
                    )),
                    DNTypeSystemExtension (TSENType (TENObjectD
                        "Obj"
                        (mnempt ["Truck", "Airplane"])
                        (nempt [Directive "spin" Nothing, Directive "jump" Nothing])
                    )),
                    DNTypeSystemExtension (TSENType (TENObjectD
                        "Obj"
                        Nothing
                        (nempt [Directive "spin" Nothing, Directive "jump" Nothing])
                    )),
                    DNTypeSystemExtension (TSENType (TENObjectI
                        "Obj"
                        (nempt ["Truck", "Airplane"])
                    )),
                    DNTypeSystemExtension (TSENType (TENObjectI
                        "Obj"
                        (nempt ["Truck", "Airplane"])
                    ))
                ],

        testParse "interface extension" [graphql|
                extend interface Ifc @spin @jump {egg : String}
                extend interface Ifc {egg: String}
                extend interface Ifc @spin @jump
                |] $
                nempt [
                    DNTypeSystemExtension (TSENType (TENInterfaceF
                        "Ifc"
                        (mnempt [Directive "spin" Nothing, Directive "jump" Nothing])
                        (nempt [
                            FieldDefinition Nothing "egg" Nothing (TNamed "String" False) Nothing
                        ])
                    )),
                    DNTypeSystemExtension (TSENType (TENInterfaceF
                        "Ifc"
                        Nothing
                        (nempt [
                            FieldDefinition Nothing "egg" Nothing (TNamed "String" False) Nothing
                        ])
                    )),
                    DNTypeSystemExtension (TSENType (TENInterfaceD
                        "Ifc"
                        (nempt [Directive "spin" Nothing, Directive "jump" Nothing])
                    ))
                ],

        testParse "union extension" [graphql|
                extend union UUU @spin @egg | One | Two
                extend union UUU @spin @egg   One | Two
                extend union UUU | One | Two
                extend union UUU   One | Two
                extend union UUU @spin @egg
                |] $
                nempt [
                    DNTypeSystemExtension (TSENType (TENUnionM
                        "UUU"
                        (mnempt [Directive "spin" Nothing, Directive "egg" Nothing])
                        (nempt ["One", "Two"])
                    )),
                    DNTypeSystemExtension (TSENType (TENUnionM
                        "UUU"
                        (mnempt [Directive "spin" Nothing, Directive "egg" Nothing])
                        (nempt ["One", "Two"])
                    )),
                    DNTypeSystemExtension (TSENType (TENUnionM
                        "UUU"
                        Nothing
                        (nempt ["One", "Two"])
                    )),
                    DNTypeSystemExtension (TSENType (TENUnionM
                        "UUU"
                        Nothing
                        (nempt ["One", "Two"])
                    )),
                    DNTypeSystemExtension (TSENType (TENUnionD
                        "UUU"
                        (nempt [Directive "spin" Nothing, Directive "egg" Nothing])
                    ))
                ],

        testParse "enum extension" [graphql|
                extend enum En @spin @egg {One Two}
                extend enum En {One Two}
                extend enum En @spin @egg
                |] $
                nempt [
                    DNTypeSystemExtension (TSENType (TENEnumV
                        "En"
                        (mnempt [Directive "spin" Nothing, Directive "egg" Nothing])
                        (nempt [
                            EnumValueDef Nothing "One" Nothing,
                            EnumValueDef Nothing "Two" Nothing
                        ])
                    )),
                    DNTypeSystemExtension (TSENType (TENEnumV
                        "En"
                        Nothing
                        (nempt [
                            EnumValueDef Nothing "One" Nothing,
                            EnumValueDef Nothing "Two" Nothing
                        ])
                    )),
                    DNTypeSystemExtension (TSENType (TENEnumD
                        "En"
                        (nempt [Directive "spin" Nothing, Directive "egg" Nothing])
                    ))
                ],

        testParse "input extension" [graphql|
                extend input In @spin @egg {One : Int Two : String!}
                extend input In {One : Int Two : String!}
                extend input In @spin @egg
                |] $
                nempt [
                    DNTypeSystemExtension (TSENType (TENInputF
                        "In"
                        (mnempt [Directive "spin" Nothing, Directive "egg" Nothing])
                        (nempt [
                            IVDN Nothing "One" (TNamed "Int" False) Nothing Nothing,
                            IVDN Nothing "Two" (TNamed "String" True) Nothing Nothing
                        ])
                    )),
                    DNTypeSystemExtension (TSENType (TENInputF
                        "In"
                        Nothing
                        (nempt [
                            IVDN Nothing "One" (TNamed "Int" False) Nothing Nothing,
                            IVDN Nothing "Two" (TNamed "String" True) Nothing Nothing
                        ])
                    )),
                    DNTypeSystemExtension (TSENType (TENInputD
                        "In"
                        (nempt [Directive "spin" Nothing, Directive "egg" Nothing])
                    ))
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

