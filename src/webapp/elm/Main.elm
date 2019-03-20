module Main exposing (init, main)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav exposing (Key, pushUrl)
import Url exposing (Url)
import Html exposing (..)
import RemoteData exposing (WebData, RemoteData(..))
import Json.Decode as D
import Json.Decode.Pipeline exposing (optional, required)
import Http exposing (emptyBody, expectJson)
import Process
import Task

type Msg = Update (WebData Person) | DoFetch | UrlChange Url | UrlRequest UrlRequest

type alias Person = { name: String, age: Int }
type alias Model = { person: WebData Person }

delay : Float -> Msg -> Cmd Msg
delay time msg =
  Process.sleep time
  |> Task.perform (\_ -> msg)

init : flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key = ( { person = NotAsked }, delay 2500 DoFetch )

personDecoder : D.Decoder Person
personDecoder = D.succeed Person
                |> required "name" D.string
                |> required "age" D.int

fetchName : Cmd Msg
fetchName =
    Http.get
      { url = "/api/json"
      , expect = expectJson (RemoteData.fromResult >> Update) personDecoder
      }

view : Model -> Html Msg
view model =
    let
        name = case model.person of
                  NotAsked  -> "Who Are You?"
                  Loading   -> "Let's find out..."
                  Success p -> "You are " ++ p.name ++ " and you are " ++ (String.fromInt p.age) ++ " years old"
                  Failure _ -> "OOPS"
    in
      div [] [ text name ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
    DoFetch       -> ( { model | person = Loading }, fetchName )
    Update person -> ( { model | person = person }, Cmd.none )
    _             -> ( model, Cmd.none )

main : Program (Maybe Bool) Model Msg
main = Browser.application
        { init = init
        , onUrlChange = UrlChange
        , onUrlRequest = UrlRequest
        , view = \model -> { title = "Haskel Everywhere", body = [ view model ] }
        , update = update
        , subscriptions = \_ -> Sub.none
        }