module Http.Request
 ( Method(..)
 , Request(..)
 , param
 , header
 , json
 ) where

import Http.Json
import Http.Json.Parser
import qualified Data.ByteString.Lazy as S
import Data.Map as Map

data Method = HEAD | OPTIONS | GET | POST | PUT | DELETE deriving (Show, Read, Eq)
data Request = Request
                { method :: Method
                , path :: String
                , headers :: Map.Map String String
                , query :: Map.Map String String
                , body :: S.ByteString
                } deriving (Show, Eq)

header :: String -> Request -> Maybe String
header name req = Map.lookup name (headers req)

param :: String -> Request -> Maybe String
param name req = Map.lookup name (query req)

json :: Request -> Either String Json
json req = parseJson (body req)