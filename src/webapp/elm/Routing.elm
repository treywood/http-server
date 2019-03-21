module Routing exposing (Route(..), fromUrl)

import Url exposing (Url)
import Url.Parser exposing ((</>), (<?>), Parser, map, oneOf, parse, s, top)


type Route
    = HomeRoute
    | OtherRoute
    | NotFoundRoute


route : Parser (Route -> a) a
route =
    oneOf
        [ map HomeRoute top
        , map OtherRoute (s "other")
        ]


fromUrl : Url -> Route
fromUrl url =
    Maybe.withDefault NotFoundRoute (parse route url)
