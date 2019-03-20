module Main exposing (init, main)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav exposing (Key, pushUrl)
import Url exposing (Url)
import Html exposing (..)


type Msg = Update | UrlChange Url | UrlRequest UrlRequest

type alias Model =
  { name : String
  }

init : flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key = ( { name = "Trey" }, Cmd.none )

view : Model -> Html Msg
view model = div [] [ text model.name ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
    _ -> ( model, Cmd.none )

main : Program (Maybe Bool) Model Msg
main = Browser.application
        { init = init
        , onUrlChange = UrlChange
        , onUrlRequest = UrlRequest
        , view = \model -> { title = "Haskel Everywhere", body = [ view model ] }
        , update = update
        , subscriptions = \_ -> Sub.none
        }