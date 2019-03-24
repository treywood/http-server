module Specs.Request (requestTests) where

import Http.Request.Parser
import Http.Request

import Data.Map as Map

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString.Lazy.Char8 as BC

requestTests :: TestTree
requestTests = testGroup "HTTP Request"
  [ testCase "parse HTTP Request" $
    let
      req = "POST /path?query=the%20value&second=one HTTP/1.1\r\nHost: www.google.com\r\nAccept: */*\r\n\r\nmessage"
    in
      (parseRequest $ BC.pack req)
        @?= Request
          { method = POST
          , path = "/path"
          , query = Map.fromList [("second","one"),("query","the value")]
          , headers = Map.fromList [("Accept","*/*"),("Host","www.google.com")]
          , body = BC.pack "message"
          }
  ]