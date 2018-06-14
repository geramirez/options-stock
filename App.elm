module App exposing (..)

import Html exposing (Html, div, text, program)
import Mouse


-- MODEL


type alias Model =
    Int


init : ( Model, Cmd Msg )
init =
    ( 0, Cmd.none )



-- MESSAGES


type Msg
    = NoOp
    | MouseMsg Mouse.Position



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text (toString model) ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMsg position ->
            ( 5000 * (position.x) - 5000 * 8, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Mouse.clicks MouseMsg



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
