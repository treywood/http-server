module Specs.Json (jsonTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Parser.Parser
import Model.Json

import qualified Data.ByteString.Char8 as BC

jsonTests :: TestTree
jsonTests = testGroup "Json"
  [ testCase "parse int" $
      (parseJson (BC.pack "12")) @?= Right (JsonInt 12)
  , testCase "parse string" $
      (parseJson (BC.pack "\"test string\"")) @?= Right (JsonString "test string")
  , testCase "parse boolean true" $
      (parseJson (BC.pack "true")) @?= Right (JsonBool True)
  , testCase "parse boolean false" $
      (parseJson (BC.pack "false")) @?= Right (JsonBool False)
  , testCase "parse null" $
      (parseJson (BC.pack "null")) @?= Right (JsonNull)
  , testCase "parse arrays" $
      (parseJson (BC.pack "[ true, 100, [ \"nested\" ] ]"))
        @?= Right (JsonArray [ JsonBool True, JsonInt 100, JsonArray [ JsonString "nested" ] ])
  , testCase "parse objects" $
      (parseJson (BC.pack "{ \"name\": \"Joe\", \"age\":30, \"nested\": { \"prop\": [ null ] } }"))
        @?= Right (JsonObject
                    [ ("name", JsonString "Joe")
                    , ("age", JsonInt 30)
                    , ("nested", JsonObject [ ("prop", JsonArray [ JsonNull ]) ])
                    ])

  , testCase "get object property" $ let
      obj = JsonObject [ ("name", JsonString "Joe"), ("age", JsonInt 30) ]
    in
      (get "name" obj) @?= Just (JsonString "Joe")
  ]