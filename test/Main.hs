module Main (main) where

import Data.Maybe (isJust)
import Functions
import Helpers
import Test.Tasty
import Test.Tasty.QuickCheck
import Type.Reflection

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Main"
    []
