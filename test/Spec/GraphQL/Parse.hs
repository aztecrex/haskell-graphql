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

    testCase "kitchenSink" $ (fst <$> kitchenSink) @?= (snd <$> kitchenSink)
    -- testCase "full query" $ do
    --     let
    --     -- when
    --         parsed = [graphql|query TestQ { amount posted }|]

    --     -- then
    --         expected = Document $
    --             (ExecutableDefinition
    --                 (OpDefExecutableDefinition
    --                     (OpTypeOperationDefinition QUERY (Just "TestQ") Nothing Nothing
    --                         (SelectionSet [
    --                             Field Nothing "amount" Nothing Nothing Nothing,
    --                             Field Nothing "posted" Nothing Nothing Nothing
    --                         ])
    --                     )
    --                 )
    --             ) :| []
    --     parsed @?= expected,

    -- testCase "unnamed query syntax" $ do
    --     let
    --     -- when
    --         parsed = [graphql|query { amount posted }|]

    --     -- then
    --         expected = Document $
    --             (ExecutableDefinition
    --                 (OpDefExecutableDefinition
    --                     (OpTypeOperationDefinition QUERY Nothing Nothing Nothing
    --                         (SelectionSet [
    --                             Field Nothing "amount" Nothing Nothing Nothing,
    --                             Field Nothing "posted" Nothing Nothing Nothing
    --                         ])
    --                     )
    --                 )
    --             ) :| []
    --     parsed @?= expected,

    -- testCase "shorthand query syntax" $ do
    --     let
    --     -- when
    --         parsed = [graphql|{ amount posted }|]

    --     -- then
    --         expected = Document $
    --             (ExecutableDefinition
    --                 (OpDefExecutableDefinition
    --                     (SelSetOperationDefinition
    --                         (SelectionSet [
    --                             Field Nothing "amount" Nothing Nothing Nothing,
    --                             Field Nothing "posted" Nothing Nothing Nothing
    --                         ])
    --                     )
    --                 )
    --             ) :| []
    --     parsed @?= expected,

    -- testCase "ignore commas" $
    --         [graphql|{amount posted customer}|] @?= [graphql|{amount, posted customer}|]


    ]

type Case = (DocumentNode, DocumentNode)

kitchenSink :: [Case]
kitchenSink = [
    (
        [graphql|query TestQ { amount posted }|],
        DNExecutable (EDNOperation (ODNTyped OT (Just "TestQ") Nothing Nothing SS)) :| []
    ),
    (
        [graphql|query { amount posted }|],
        DNExecutable (EDNOperation (ODNTyped OT Nothing Nothing Nothing SS)) :| []
    ),
    (
        [graphql|{ amount posted }|],
        DNExecutable (EDNOperation (ODNSelectionSet SS)) :| []
    )
    ]


