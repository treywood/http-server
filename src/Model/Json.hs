module Model.Json
 ( Json(..)
 , serialize
 ) where

import Data.List

data Json = JsonInt Int | JsonString String | JsonBool Bool | JsonArray [Json] | JsonNull | JsonObject [(String, Json)] deriving (Show)

serialize :: Json -> String
serialize (JsonInt i) = show i
serialize (JsonString s) = "\"" ++ s ++ "\""
serialize JsonNull = "null"
serialize (JsonBool True) = "true"
serialize (JsonBool False) = "false"
serialize (JsonArray arr) = "[" ++ (intercalate "," $ map serialize arr) ++ "]"
serialize (JsonObject fields) = "{" ++ (intercalate "," $ map fmt fields) ++ "}"
  where
    fmt :: (String, Json) -> String
    fmt (name, val) = name ++ ":" ++ (serialize val)
