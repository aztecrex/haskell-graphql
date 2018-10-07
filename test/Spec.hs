{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE QuasiQuotes  #-}

import Test.Tasty (TestTree, testGroup, defaultMain)
import qualified Spec.Language as Language (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "all" [
    Language.tests
    ]
