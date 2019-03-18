module Lib
    ( handleRequest
    ) where

import Parser.Request
import qualified Model.Request as Req
import Model.Response

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as BC
import Data.List
import Data.List.Split

route :: Req.Method -> [String] -> Req.Request -> Response

route Req.POST ["api", "path"] req =
  let
    method' = show $ Req.method req
    path' = Req.path req
    headers' = show $ Req.headers req
    body' = Req.body req
    msg = "You sent a " ++ method' ++ " request to " ++ path' ++ " with headers: " ++ headers' ++ " and body: " ++ body'
  in
    respond (BC.pack msg)

route Req.GET ["api", "greet", name] req =
  let
    it = if (name == "kyle") then
      Nothing
    else
      Just (BC.pack $ "hello " ++ name)
  in
    respond it

route _ _ _ = Response
  { status = 404
  , headers = [("Content-Type", "text/plain")]
  , body = BC.pack "Not Found"
  }

handleRequest :: S.ByteString -> S.ByteString
handleRequest msg =
  let
    req = parseRequest msg
    pathParts = tail $ splitOn "/" (Req.path req)
    res = route (Req.method req) pathParts req
  in
    serialize res