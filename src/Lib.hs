module Lib
    ( handleRequest
    ) where

import Parser.Request
import qualified Model.Request as Req
import Model.Response
import qualified Model.Json as Json

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
    body' = BC.unpack (Req.body req)
  in
    respond $ "You sent a " ++ method' ++ " request to " ++ path' ++ " with headers: " ++ headers' ++ " and body: " ++ body'

route Req.GET ["api", "greet", name] req =
  let
    it = if (name == "kyle") then
      Nothing
    else
      Just ("hello " ++ name)
  in
    respond it

route Req.GET ["api", "json"] _ =
  respond $ Json.JsonObject [("name", Json.JsonString "Trey"), ("age", Json.JsonInt 30)]

route _ _ _ =
  respond (Nothing :: Maybe String)

handleRequest :: S.ByteString -> S.ByteString
handleRequest msg =
  let
    req = parseRequest msg
    pathParts = tail $ splitOn "/" (Req.path req)
    res = route (Req.method req) pathParts req
  in
    serialize res