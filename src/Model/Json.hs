module Model.Json
 ( Json(..)
 , JsonField
 , serializeJson
 ) where

import Data.List

type JsonField = (String, Json)
data Json = JsonInt Int | JsonString String | JsonBool Bool | JsonArray [Json] | JsonNull | JsonObject [JsonField] deriving (Show)

serializeJson :: Json -> String
serializeJson (JsonInt i) = show i
serializeJson (JsonString s) = "\"" ++ s ++ "\""
serializeJson JsonNull = "null"
serializeJson (JsonBool True) = "true"
serializeJson (JsonBool False) = "false"
serializeJson (JsonArray arr) = "[" ++ (intercalate "," $ map serializeJson arr) ++ "]"
serializeJson (JsonObject fields) = "{" ++ (intercalate "," $ map fmt fields) ++ "}"
  where
    fmt :: JsonField -> String
    fmt (name, val) = "\"" ++ name ++ "\":" ++ (serializeJson val)
