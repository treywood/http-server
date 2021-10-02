module Main (main) where

import           Http.Json
import           Http.Json.Parser
import qualified Http.Request               as Req
import           Http.Response
import           Http.Server

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BC
import           Data.List
import           System.IO

import           Network.HTTP.Client

import qualified Html                       as H
import qualified Html.Attr                  as A

route :: Routes
route Req.POST ["api", "path"] req =
  let
    method' = show $ Req.method req
    path' = Req.path req
    headers' = show $ Req.headers req
    body' = BC.unpack (Req.body req)
  in
    respond $
      "You sent a " ++ method' ++ " request to " ++ path' ++
      " with headers: " ++ headers' ++ " and body: " ++ body'

route Req.GET ["api", "greet", name] req =
  let
    it = if name == "kyle" then
      Nothing
    else
      Just ("hello " ++ name)
  in
    respond it

route Req.GET ["api", "msg"] req =
  maybe
    (respond (400, "Send a message dangit"))
    (\m -> respond $ JsonObject [("msg", JsonString m)])
    (Req.param "msg" req)

route Req.GET ["api", "json"] _ = do
    threadDelay 2500000
    respond $ JsonObject
      [ ("name", JsonString "Trey")
      , ("age", JsonInt 30)
      ]

route Req.POST ["api", "json"] req =
  case Req.json req of
    Right json ->
      case get "name" json of
        Just (JsonString name) -> respond $ "Your name is " ++ name
        Just v                 -> respond $ "Your name is weird: (" ++ show v ++ ")"
        _                      -> respond "You don't have a name I guess"

    Left err ->
      respond (400, "Bad JSON: " ++ err)

route Req.GET ("assets" : path) _ =
  static "target" (intercalate "/" path)

route Req.GET ["html"] _ =
  respond $ H.html []
    [ H.head []
      [ H.title [] [H.text "TEST"]
      ]
    , H.body []
      [ H.a [A.href "https://www.google.com"] [H.text "link"]
      , H.form [A.action ""]
        [ H.input [A.type_ "text", A.name "text"]
        , H.button [A.type_ "submit"] [H.text "submit"]
        ]
      ]
    ]

route Req.GET ["time"] _ =
  respond $ do
    manager <- newManager defaultManagerSettings
    request <- parseRequest "http://worldtimeapi.org/api/ip"
    response <- httpLbs request manager
    let responseStr = responseBody response

    putStrLn $ BC.unpack responseStr

    case parseJson responseStr of
      Right json ->
        case get "timezone" json of
          Just (JsonString timezone) ->
            return $ H.html []
              [ H.div [] [H.text $ "Your timezone is " ++ timezone]
              ]
          _ ->
            return $ H.html []
              [ H.div [] [H.text "Your timezone is missing"]
              ]

      Left error ->
        return $ H.html []
          [ H.div [] [H.text $ "ERROR: " ++ error]
          ]

route Req.GET _ _ =
  sendFile "target/index.html"

route _ _ _ =
  respond notFoundResponse

main :: IO ()
main = startServer $ defaultOptions { routes = route }

