module Hammurabi exposing (main)

import Browser
import Html exposing (Html, br, button, div, input, label, p, text)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (placeholder, type_, value)
import Random
import Result exposing (andThen)

acresPerWorker : Int
acresPerWorker = 10

bushelsPerAcre : Float
bushelsPerAcre = 0.5

minYieldBpa : Int
minYieldBpa = 1

maxYieldBpa : Int
maxYieldBpa = 5

type State
    = Playing
    | Finished

type alias Model = 
    { state : State
    , error : Maybe String
    , rnd : Random.Seed
    , year : Int
    , impeached : Bool
    , population : Int
    , deaths : Int
    , totalDeaths : Int
    , avgDeaths : Float
    , births : Int
    , plague : Bool
    , acres : Int
    , planted : Int
    , yield : Int
    , consumed : Int
    , devoured : Int
    , bushels : Int
    , price : Int
    , buy : String
    , sell : String
    , feed : String
    , plant : String
    }

init : Model
init =
    let
        rnd0 = Random.initialSeed 42
        (price, rnd1) = Random.step (Random.int 17 26) rnd0
    in
        { state = Playing
        , error = Nothing
        , rnd = rnd1
        , year = 1
        , impeached = False
        , population = 100
        , deaths = 0
        , totalDeaths = 0
        , avgDeaths = 0
        , births = 5
        , plague = False
        , acres = 1000
        , planted = 0
        , yield = 3
        , consumed = 0
        , devoured = 200
        , bushels = 2800
        , price = price
        , buy = "0"
        , sell = "0"
        , feed = "0"
        , plant = "0"
        }

type Msg
    = Restart
    | DoIt
    | Buy String
    | Sell String
    | Feed String
    | Plant String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Restart ->
            init
        DoIt ->
            step model
        Buy s ->
            { model | buy = s }
        Sell s ->
            { model | sell = s }
        Feed s ->
            { model | feed = s }
        Plant s ->
            { model | plant = s }

step : Model -> Model
step model =
    let
        buyA = Maybe.withDefault 0 (String.toInt model.buy)
        sellA = Maybe.withDefault 0 (String.toInt model.sell)
        feedA = Maybe.withDefault 0 (String.toInt model.feed)
        plantA = Maybe.withDefault 0 (String.toInt model.plant)
        newModel = 
            Ok model 
                |> andThen (buy buyA)
                |> andThen (sell sellA)
                |> andThen (feed feedA)
                |> andThen (plant plantA)
                |> andThen rats
                |> andThen harvest
                |> andThen life
                |> andThen plague
                |> andThen endYear
    in
        case newModel of
            Ok m ->
                { m
                | error = Nothing
                }
            Err str ->
                { model
                | error = Just str
                }

buy : Int -> Model -> Result String Model
buy amount model =
    if amount * model.price > model.bushels then
        Err <| "Hammurabi: Think again. You own only " ++  String.fromInt model.bushels ++ " bushels of grain. Now then,"
    else
        Ok 
            { model
            | acres = model.acres + amount
            , bushels = model.bushels - amount * model.price
            }

sell : Int -> Model -> Result String Model
sell amount model =
    if amount > model.acres then
        Err <| "Hammurabi: Think again. You own only " ++  String.fromInt model.acres ++ " acres. Now then,"
    else
        Ok 
            { model
            | acres = model.acres - amount
            , bushels = model.bushels + amount * model.price
            }

feed : Int -> Model -> Result String Model
feed amount model =
    if amount > model.bushels then
        Err <| "Hammurabi: Think again. You own only " ++  String.fromInt model.bushels ++ " bushels of grain. Now then,"
    else
        Ok 
            { model
            | bushels = model.bushels - amount
            , consumed = amount
            }

plant : Int -> Model -> Result String Model
plant amount model =
    if amount > model.acres then
        Err <| "Hammurabi: Think again. You own only " ++  String.fromInt model.acres ++ " acres. Now then,"
    else if round (toFloat amount * bushelsPerAcre) > model.bushels then
        Err <| "Hammurabi: Think again. You own only " ++  String.fromInt model.bushels ++ " bushels of grain. Now then,"
    else if amount > model.population * acresPerWorker then
        Err <| "But you have only " ++  String.fromInt model.population ++ " people to tend the fields! Now then,"
    else
        Ok 
            { model
            | planted = amount
            , bushels = model.bushels - round (toFloat amount * bushelsPerAcre)
            }

rats : Model -> Result String Model
rats model =
    let
        (luck, rnd1) = Random.step (Random.int 1 5) model.rnd
        devoured = if remainderBy 2 luck == 0 then 0 else model.bushels // luck
    in
        Ok 
            { model
            | rnd = rnd1
            , devoured = devoured
            , bushels = model.bushels - devoured
            }

harvest : Model -> Result String Model
harvest model =
    let
        (yield, rnd1) = Random.step (Random.int minYieldBpa maxYieldBpa) model.rnd
        harvested = model.planted * yield
    in
        Ok 
            { model
            | rnd = rnd1
            , yield = yield
            , bushels = model.bushels + harvested
            }

life : Model -> Result String Model
life model =
    let
        (luck, rnd1) = Random.step (Random.int 1 5) model.rnd
        births = luck * (20 * model.acres + model.bushels) // model.population // 100 + 1
        deaths = max 0 (model.population - model.consumed // 20)
        avgDeaths = (toFloat (model.year - 1) * model.avgDeaths + 
                     toFloat (model.deaths * 100) / toFloat model.population) / toFloat model.year
    in
        Ok 
            { model
            | rnd = rnd1
            , births = births
            , deaths = deaths
            , totalDeaths = model.totalDeaths + deaths
            , avgDeaths = avgDeaths
            , population = model.population + births - deaths
            , impeached = (toFloat deaths) > 0.45 * (toFloat model.population)
            }

plague : Model -> Result String Model
plague model =
    let
        (luck, rnd1) = Random.step (Random.float 0 1) model.rnd
        strikes = luck < 0.15
        population = if strikes then model.population // 2 else model.population
    in
        Ok 
            { model
            | rnd = rnd1
            , population = population
            , plague = strikes
            }

endYear : Model -> Result String Model
endYear model =
    let
        (price, rnd1) = Random.step (Random.int 17 26) model.rnd
    in
        Ok 
            { model
            | year = model.year + 1
             , rnd = rnd1
            , price = price
            , state = if model.year == 10 || model.impeached then Finished else Playing
            }

view : Model -> Html Msg
view model =
    case model.state of
        Playing ->
            viewPlaying model
        Finished ->
            viewFinished model

viewPlaying : Model -> Html Msg
viewPlaying model =
    div []
        [ div [] [ viewState model ]
        , div [] [ viewPrice model ]
        , div []
            [ labelledInput "Buy" model.buy Buy
            , labelledInput "Sell" model.sell Sell
            , labelledInput "Feed" model.feed Feed
            , labelledInput "Plant" model.plant Plant
            ]
        , button [ onClick DoIt ] [ text "Let it be done" ]
        , div []
            [ case model.error of
                Nothing ->
                    p [] []
                Just msg ->
                    p [] [ text msg ]
            ]
        ]

labelledInput : String -> String -> (String -> Msg) -> Html Msg
labelledInput lbl val msg =
    label []
        [ text lbl
        , input [ type_ "text", placeholder lbl, value val, onInput msg ] []
        ]

viewState : Model -> Html Msg
viewState model =
    p []
        [ text "Hammurabi: I beg to report to you,"
        , br [] []
        , text <| "In year " ++ String.fromInt model.year ++ ", "
                  ++ String.fromInt model.deaths ++ " people starved, " 
                  ++ String.fromInt model.births ++ " came to the city." 
        , br [] []
        , if model.plague then
            text <| "A horrible plague struck! Half the people died." 
          else 
            text ""
        , if model.plague then br [] [] else text ""
        , text <| "The city population is now " ++ String.fromInt model.population ++ "."
        , br [] []
        , text <| "The city now owns " ++ String.fromInt model.acres ++ " acres."
        , br [] []
        , text <| "You harvested " ++ String.fromInt model.yield ++ " bushels per acre."
        , br [] []
        , text <| "Rats ate " ++ String.fromInt model.devoured ++ " bushels."
        , br [] []
        , text <| "You now have " ++ String.fromInt model.bushels ++ " bushels in store."
        ]

viewPrice : Model -> Html Msg
viewPrice model =
    p [] [ text <| "Land is trading at " ++ String.fromInt model.price ++ " bushels per acre." ]


viewFinished : Model -> Html Msg
viewFinished model =
    div []
        [ div [] [ viewFinalReport model ]
        , button [ onClick Restart ] [ text "Play again" ]
        ]

viewFinalReport : Model -> Html Msg
viewFinalReport model =
    if model.impeached then
        p [] 
            [ text <| "You starved " ++ String.fromInt model.deaths ++ " people in one year!!!"
            , br [] []
            , text "Due to this extreme mismanagement you have not only been impeached "
            , text "and thrown out of office but you have also been declared national fink!!!"
            , br [] []
            ]
    else
        let
            app = toFloat model.acres / toFloat model.population
            (luck, rnd1) = Random.step (Random.float 0 1) model.rnd
            haters = round (toFloat model.population * 0.8 * luck)
        in
            p []
                [ text <| "In your ten-year term of office, " ++ String.fromFloat model.avgDeaths ++ 
                    " percent of the population starved per year on average, i.e. a total of " ++ String.fromInt model.totalDeaths ++
                    " people died."
                , br [] []
                , text <| "You started with 10 acres per person and ended with " ++ String.fromFloat app ++ " acres per person."
                , br [] []
                , if model.avgDeaths > 33 || app < 7 then
                    p [] 
                        [ text "Due to this extreme mismanagement you have not only been impeached "
                        , text "and thrown out of office but you have also been declared national fink!!!"
                        ]
                  else if model.avgDeaths > 10 || app < 9 then
                    p [] 
                        [ text "Your heavy-handed performance smacks of Nero and Ivan IV. "
                        , br [] []
                        , text "The people (remaining) find you an unpleasant ruler, and, frankly, hate your guts!!"
                        ]
                  else if model.avgDeaths > 3 || app < 10 then
                    p [] 
                        [ text "Your performance could be somewhat better, but really wasn't too bad at all."
                        , br [] []
                        , text <| String.fromInt haters ++ " people dearly want to see you assasinated but we all have our trivial problems."
                        ]
                  else
                    text "A fantastic performance!!! Charlemagne, Disraeli, and Jefferson combined could not have done better"
                ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }