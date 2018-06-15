module App exposing (..)

import Html exposing (Html, div, text, program, h1, input, label)
import Html.Attributes exposing (defaultValue, type_)
import Html.Events exposing (onInput)
import Time exposing (Time, second)
import Http


-- MODEL


type alias Model =
    { stockPrice : Float
    , availableOptions : Float
    , initialOfferingValue : Float
    , taxRate : Float
    , tickerSymbol : String
    }


type alias EarningsData =
    { totalEarning : String
    , cost : String
    , netGain : String
    , taxRate : String
    }


init : ( Model, Cmd Msg )
init =
    ( { stockPrice = 0.0
      , availableOptions = 5000.0
      , initialOfferingValue = 8.5
      , taxRate = 0
      , tickerSymbol = "pvtl"
      }
    , Cmd.none
    )



-- MESSAGES


type Inputs
    = InitialOfferingValue
    | AvailableOptions
    | TaxRate
    | TickerSymbol


type Msg
    = NoOp
    | NewStockPrice (Result Http.Error String)
    | InputChange Inputs String
    | Tick Time



-- VIEW


view : Model -> Html Msg
view model =
    let
        earningsData =
            calculateEarnings model
    in
        div []
            [ h1 [] [ text ("Earnings: " ++ earningsData.netGain) ]
            , div []
                [ text ("Cost: " ++ earningsData.cost)
                , text ("Total Earnings: " ++ earningsData.totalEarning)
                , text ("Tax Rate: " ++ earningsData.taxRate)
                , text ("Stock Price: " ++ (toString model.stockPrice))
                , text ("Ticker: " ++ model.tickerSymbol)
                ]
            , label []
                [ text "Initial Offering Value"
                , input
                    [ onInput (InputChange InitialOfferingValue)
                    , type_ "number"
                    , defaultValue (toString model.initialOfferingValue)
                    ]
                    []
                ]
            , label []
                [ text "Available Options"
                , input
                    [ onInput (InputChange AvailableOptions)
                    , type_ "number"
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
            , label []
                [ text "Tax Rate"
                , input
                    [ onInput (InputChange TaxRate)
                    , type_ "number"
                    , defaultValue (toString model.taxRate)
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

                TaxRate ->
                    ( { model | taxRate = (String.toFloat inputText |> Result.withDefault 0.0) }, Cmd.none )

                TickerSymbol ->
                    ( { model | tickerSymbol = inputText }, Cmd.none )

        Tick newTime ->
            ( model, getStockData model.tickerSymbol )

        NoOp ->
            ( model, Cmd.none )


calculateEarnings : Model -> EarningsData
calculateEarnings model =
    let
        totalEarning =
            (model.availableOptions * model.stockPrice)

        cost =
            (model.availableOptions * model.initialOfferingValue)

        netGainBeforeTax =
            totalEarning - cost

        netGain =
            netGainBeforeTax * (1 - model.taxRate)
    in
        { totalEarning = totalEarning |> toString
        , cost = cost |> toString
        , netGain = netGain |> toString
        , taxRate = model.taxRate |> toString
        }



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
