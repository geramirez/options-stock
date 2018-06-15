module App exposing (..)

import Html exposing (Html, div, text, program, h1, input, label)
import Html.Attributes exposing (defaultValue)
import Html.Events exposing (onInput)
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


type Inputs
    = InitialOfferingValue
    | AvailableOptions
    | TickerSymbol


type Msg
    = NoOp
    | NewStockPrice (Result Http.Error String)
    | InputChange Inputs String
    | Tick Time



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text (calculateEarnings model) ]
        , label []
            [ text "Initial Offering Value"
            , input
                [ onInput (InputChange InitialOfferingValue)
                , defaultValue (toString model.initialOfferingValue)
                ]
                []
            ]
        , label []
            [ text "Available Options"
            , input
                [ onInput (InputChange AvailableOptions)
                , defaultValue (toString model.availableOptions)
                ]
                []
            ]
        , label []
            [ text "Ticker Symbol"
            , input
                [ onInput (InputChange TickerSymbol)
                , defaultValue model.tickerSymbol
                ]
                []
            ]
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewStockPrice httpstatus ->
            case httpstatus of
                Ok stringPrice ->
                    ( { model | stockPrice = (String.toFloat stringPrice |> Result.withDefault 0.0) }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        InputChange inputType inputText ->
            case inputType of
                InitialOfferingValue ->
                    ( { model | initialOfferingValue = (String.toFloat inputText |> Result.withDefault 0.0) }, Cmd.none )

                AvailableOptions ->
                    ( { model | availableOptions = (String.toFloat inputText |> Result.withDefault 0.0) }, Cmd.none )

                TickerSymbol ->
                    ( { model | tickerSymbol = inputText }, Cmd.none )

        Tick newTime ->
            ( model, getStockData model.tickerSymbol )

        NoOp ->
            ( model, Cmd.none )


calculateEarnings : Model -> String
calculateEarnings model =
    let
        totalEarning =
            (model.availableOptions * model.stockPrice)

        cost =
            (model.availableOptions * model.initialOfferingValue)

        netGain =
            totalEarning - cost
    in
        netGain
            |> round
            |> toString



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (second * 10) Tick



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
        Http.send NewStockPrice request
