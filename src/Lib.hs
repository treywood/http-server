module Lib
    ( handleRequest
    ) where

import Parser.Request
import Model.Request as Req
import Model.Response as Res

import qualified Data.ByteString as S

handleRequest :: S.ByteString -> S.ByteString
handleRequest msg =
  let
    req = parseRequest msg
    method' = show $ Req.method req
    path' = Req.path req
    headers' = show $ Req.headers req
    body' = Req.body req
  in
    Res.serialize Res.Response
      { Res.status = 200
      , Res.headers = [("Content-Type", "text/plain")]
      , Res.body = "You sent a " ++ method' ++ " request to " ++ path' ++ " with headers: " ++ headers' ++ " and body: " ++ body'
      }