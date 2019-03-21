module Main exposing (init, main)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav exposing (Key)
import Html exposing (..)
import Http exposing (expectJson)
import Json.Decode as D
import Json.Decode.Pipeline exposing (required)
import Process
import RemoteData exposing (RemoteData(..), WebData)
import Routing exposing (Route(..), fromUrl)
import Task
import Url exposing (Url)


type Msg
    = Update (WebData Person)
    | DoFetch
    | UrlChange Url
    | UrlRequest UrlRequest


type alias Person =
    { name : String, age : Int }


type alias Model =
    { route : Route, key : Key, person : WebData Person }


delay : Float -> Msg -> Cmd Msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


init : flags -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    ( { route = fromUrl url
      , person = NotAsked
      , key = key
      }
    , delay 2500 DoFetch
    )


personDecoder : D.Decoder Person
personDecoder =
    D.succeed Person
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
    case model.route of
        HomeRoute ->
            let
                msg =
                    case model.person of
                        NotAsked ->
                            "Who Are You?"

                        Loading ->
                            "Let's find out..."

                        Success p ->
                            "You are " ++ p.name ++ " and you are " ++ String.fromInt p.age ++ " years old"

                        Failure _ ->
                            "OOPS"
            in
            div [] [ text msg ]

        OtherRoute ->
            div [] [ text "It's other, what?" ]

        NotFoundRoute ->
            div [] [ text "There's nothing here" ]


locationChange : Url -> Model -> ( Model, Cmd Msg )
locationChange url model =
    let
        newRoute =
            fromUrl url

        newModel =
            { model | route = newRoute }
    in
    case newRoute of
        _ ->
            ( newModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoFetch ->
            ( { model | person = Loading }, fetchName )

        Update person ->
            ( { model | person = person }, Cmd.none )

        UrlChange url ->
            locationChange url model

        UrlRequest req ->
            case req of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )


main : Program (Maybe Bool) Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = UrlChange
        , onUrlRequest = UrlRequest
        , view = \model -> { title = "Haskell Everywhere", body = [ view model ] }
        , update = update
        , subscriptions = \_ -> Sub.none
        }
