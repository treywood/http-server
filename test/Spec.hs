module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Specs.Json
import Specs.Request

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [jsonTests, requestTests]
