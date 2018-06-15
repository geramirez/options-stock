module App exposing (..)

import Html exposing (Html, div, text, program)
import Time exposing (Time, second)
import Http


-- MODEL


type alias Model =
    { earnings : Float
    , stockPrice : Float
    , availableOptions : Float
    , initialOfferingValue : Float
    , tickerSymbol : String
    }


init : ( Model, Cmd Msg )
init =
    ( { earnings = 0.0
      , stockPrice = 0.0
      , availableOptions = 5000.0
      , initialOfferingValue = 8.5
      , tickerSymbol = "pvtl"
      }
    , Cmd.none
    )



-- MESSAGES


type Msg
    = NoOp
    | NewMessage (Result Http.Error String)
    | Tick Time



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text (toString (round model.earnings)) ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewMessage (Ok stringPrice) ->
            ( { model | earnings = (calculateEarnings stringPrice model) }, Cmd.none )

        NewMessage (Err _) ->
            ( model, Cmd.none )

        Tick newTime ->
            ( { model | earnings = 0 }, getStockData model.tickerSymbol )

        NoOp ->
            ( model, Cmd.none )


calculateEarnings : String -> Model -> Float
calculateEarnings stringPrice model =
    let
        floatPrice =
            (String.toFloat stringPrice |> Result.withDefault 0.0)
    in
        (model.availableOptions * floatPrice) - (model.availableOptions * model.initialOfferingValue)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (second * 30) Tick



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


getStockData : String -> Cmd Msg
getStockData tickerSymbol =
    let
        url =
            "https://api.iextrading.com/1.0/stock/" ++ tickerSymbol ++ "/price"

        request =
            Http.getString url
    in
        Http.send NewMessage request
