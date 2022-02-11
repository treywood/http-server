module Http.Request
  ( Method(..)
  , Request(..)
  , param
  , header
  , json
  ) where

import qualified Data.ByteString.Lazy as S
import           Data.Map             as Map
import           Http.Json
import           Http.Json.Parser

data Method
  = HEAD
  | OPTIONS
  | GET
  | POST
  | PUT
  | DELETE
  deriving (Show, Read, Eq)

data Request =
  Request
    { method  :: Method
    , path    :: String
    , headers :: Map.Map String String
    , query   :: Map.Map String String
    , body    :: S.ByteString
    }
  deriving (Show, Eq)

header :: String -> Request -> Maybe String
header name = Map.lookup name . headers

param :: String -> Request -> Maybe String
param name = Map.lookup name . query

json :: Request -> Either String Json
json = parseJson . body
