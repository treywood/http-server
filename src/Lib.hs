module Lib
    ( handleRequest
    ) where

import Parser.Request
import Parser.Json
import qualified Model.Request as Req
import Model.Response
import Model.Json

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
  respond $ JsonObject [("name", JsonString "Trey"), ("age", JsonInt 30)]

route Req.POST ["api", "json"] req =
  case parseJson (Req.body req) of
    Right json ->
      case get "name" json of
        Just (JsonString name) -> respond $ "Your name is " ++ name
        Just v                 -> respond $ "Your name is weird: (" ++ (show v) ++ ")"
        _                      -> respond $ "You don't have a name I guess"

    Left err -> Response
      { status = 400
      , headers = [("Content-Type", "text/plain")]
      , body = BC.pack $ "Bad JSON: " ++ err
      }

route Req.GET [] _ =
  respond ()

route _ _ _ =
  respond (Nothing :: Maybe ())

handleRequest :: S.ByteString -> S.ByteString
handleRequest msg =
  let
    req = parseRequest msg
    pathParts = tail $ splitOn "/" (Req.path req)
    res = route (Req.method req) pathParts req
  in
    serializeResponse res