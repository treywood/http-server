module Lib
    ( handleRequest
    ) where

import Parser.Request
import qualified Model.Request as Req
import Model.Response

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
    serialize Response
      { status = 200
      , headers = [("Content-Type", "text/plain")]
      , body = "You sent a " ++ method' ++ " request to " ++ path' ++ " with headers: " ++ headers' ++ " and body: " ++ body'
      }