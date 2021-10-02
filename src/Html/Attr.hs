module Html.Attr
  ( Html.Attr.class_
  , Html.Attr.name
  , Html.Attr.type_
  , Html.Attr.href
  , Html.Attr.target
  , Html.Attr.action
  , Html.Attr.checked
  ) where

import           Data.List (intercalate)
import           Html      (Attribute)

attr :: String -> String -> Attribute
attr name value = (name, value)

class_ :: [String] -> Attribute
class_ names = attr "class" $ unwords names

type_ :: String -> Attribute
type_ = attr "type"

name :: String -> Attribute
name = attr "name"

href :: String -> Attribute
href = attr "href"

target :: String -> Attribute
target = attr "target"

checked :: Bool -> Attribute
checked b =
  attr "checked" $
  if b
    then "checked"
    else ""

action :: String -> Attribute
action = attr "action"

