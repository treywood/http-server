module Model.Response
 ( Response(..)
 , serialize
 ) where

import qualified Model.Headers as H

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as BC

data Response = Response { status :: Int, headers :: H.Headers, body :: String }

serializeCode :: Int -> String
serializeCode 200 = "200 OK"
serializeCode c = show c

serialize :: Response -> S.ByteString
serialize res =
  let
    head = BC.unlines $ map BC.pack
      [ "HTTP/1.1 " ++ serializeCode (status res)
      , H.serialize (headers res)
      , "Content-Length: " ++ (show $ length (body res))
      , ""
      ]
  in
    BC.append head (BC.pack $ body res)