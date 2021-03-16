module Html
  ( Element
  , Attribute
  , Html.tag
  , Html.html
  , Html.head
  , Html.title
  , Html.link
  , Html.meta
  , Html.script
  , Html.style
  , Html.body
  , Html.a
  , Html.div
  , Html.form
  , Html.input
  , Html.button
  , Html.ul
  , Html.ol
  , Html.li
  , Html.text
  ) where

import Data.List (intercalate)

type Attribute = (String, String)

data Element =
    Element String [Attribute] [Element] 
  | LeafElement String [Attribute]
  | Text String

tag :: String -> [Attribute] -> [Element] -> Element
tag name attrs children = Element name attrs children

leafTag :: String -> [Attribute] -> Element
leafTag name attrs = LeafElement name attrs

html = tag "html"

head = tag "head"

title = tag "title"

link = tag "link"

meta = tag "meta"

script = tag "script"

style = tag "style"

body = tag "body"

a = tag "a"

div = tag "div"

form = tag "form"

input = leafTag "input"

button = tag "button"

ul = tag "ul"

ol = tag "ol"

li = tag "li"

text value = Text value

printEl :: Element -> String
printEl el = printIndent 0 el
  where
    printIndent i (Element name attrs children) =
      (tabs i) ++ "<" ++ name ++ (printAttrs attrs) ++ ">" ++ 
        (printChildren (i + 1) children) ++ 
      (tabs i) ++ "</" ++ name ++ ">"

    printIndent i (LeafElement name attrs) =
      (tabs i) ++ "<" ++ name ++ (printAttrs attrs) ++ "/>"
    
    printIndent i (Text str) = (tabs i) ++ str

    tabs i = (concat $ replicate i "  ")

    printAttrs [] = ""
    printAttrs attrs = " " ++ (intercalate " " $ map printAttr attrs)

    printAttr (name, value) = name ++ "=\"" ++ value ++ "\""
    
    printChildren _ [] = ""
    printChildren i children =
      "\n" ++ (intercalate "\n" $ map (printIndent i) children) ++ "\n"

instance Show Element where
  show el = printEl el
