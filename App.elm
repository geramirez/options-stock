module App exposing (..)

import Html exposing (Html, div, text, program)
import Mouse


-- MODEL


type alias Model =
    { earnings : Int, stockPrice : Int, availableOptions : Int, initialOfferingValue : Int }


init : ( Model, Cmd Msg )
init =
    ( { earnings = 0, stockPrice = 0, availableOptions = 5000, initialOfferingValue = 8 }, Cmd.none )



-- MESSAGES


type Msg
    = NoOp
    | MouseMsg Mouse.Position



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text (toString model.earnings) ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMsg position ->
            ( { model | earnings = model.availableOptions * (position.x) - model.availableOptions * model.initialOfferingValue }, Cmd.none )

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
