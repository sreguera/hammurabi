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

type Model
    = Playing Form State
    | Finished State

type alias State = 
    { rnd : Random.Seed
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
    }

type alias Form =
    { error : Maybe String
    , buy : String
    , sell : String
    , feed : String
    , plant : String
    }

init : Model
init = Playing initForm initState

initForm : Form
initForm =
    { error = Nothing
    , buy = "0"
    , sell = "0"
    , feed = "0"
    , plant = "0"
    }

initState : State
initState =
    let
        rnd0 = Random.initialSeed 42
        (price, rnd1) = Random.step (Random.int 17 26) rnd0
    in
        { rnd = rnd1
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
    case (msg, model) of
        (Restart, _) ->
            init
        (DoIt, Playing form state) ->
            step form state
        (Buy s, Playing form state) ->
            Playing { form | buy = s } state
        (Sell s, Playing form state) ->
            Playing { form | sell = s } state
        (Feed s, Playing form state) ->
            Playing { form | feed = s } state
        (Plant s, Playing form state) ->
            Playing { form | plant = s } state
        (_, _) ->
            model

step : Form -> State -> Model
step form state =
    let
        buyA = Maybe.withDefault 0 (String.toInt form.buy)
        sellA = Maybe.withDefault 0 (String.toInt form.sell)
        feedA = Maybe.withDefault 0 (String.toInt form.feed)
        plantA = Maybe.withDefault 0 (String.toInt form.plant)
        resultState = 
            Ok state 
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
        case resultState of
            Ok nextState ->
                if nextState.year == 11 || nextState.impeached then 
                    Finished nextState 
                else
                    Playing initForm nextState
            Err str ->
                Playing
                    { form
                    | error = Just str
                    }
                    state

buy : Int -> State -> Result String State
buy amount state =
    if amount * state.price > state.bushels then
        Err <| "Hammurabi: Think again. You own only " ++  String.fromInt state.bushels ++ " bushels of grain. Now then,"
    else
        Ok 
            { state
            | acres = state.acres + amount
            , bushels = state.bushels - amount * state.price
            }

sell : Int -> State -> Result String State
sell amount state =
    if amount > state.acres then
        Err <| "Hammurabi: Think again. You own only " ++  String.fromInt state.acres ++ " acres. Now then,"
    else
        Ok 
            { state
            | acres = state.acres - amount
            , bushels = state.bushels + amount * state.price
            }

feed : Int -> State -> Result String State
feed amount state =
    if amount > state.bushels then
        Err <| "Hammurabi: Think again. You own only " ++  String.fromInt state.bushels ++ " bushels of grain. Now then,"
    else
        Ok 
            { state
            | bushels = state.bushels - amount
            , consumed = amount
            }

plant : Int -> State -> Result String State
plant amount state =
    if amount > state.acres then
        Err <| "Hammurabi: Think again. You own only " ++  String.fromInt state.acres ++ " acres. Now then,"
    else if round (toFloat amount * bushelsPerAcre) > state.bushels then
        Err <| "Hammurabi: Think again. You own only " ++  String.fromInt state.bushels ++ " bushels of grain. Now then,"
    else if amount > state.population * acresPerWorker then
        Err <| "But you have only " ++  String.fromInt state.population ++ " people to tend the fields! Now then,"
    else
        Ok 
            { state
            | planted = amount
            , bushels = state.bushels - round (toFloat amount * bushelsPerAcre)
            }

rats : State -> Result String State
rats state =
    let
        (luck, rnd1) = Random.step (Random.int 1 5) state.rnd
        devoured = if remainderBy 2 luck == 0 then 0 else state.bushels // luck
    in
        Ok 
            { state
            | rnd = rnd1
            , devoured = devoured
            , bushels = state.bushels - devoured
            }

harvest : State -> Result String State
harvest state =
    let
        (yield, rnd1) = Random.step (Random.int minYieldBpa maxYieldBpa) state.rnd
        harvested = state.planted * yield
    in
        Ok 
            { state
            | rnd = rnd1
            , yield = yield
            , bushels = state.bushels + harvested
            }

life : State -> Result String State
life state =
    let
        (luck, rnd1) = Random.step (Random.int 1 5) state.rnd
        births = luck * (20 * state.acres + state.bushels) // state.population // 100 + 1
        deaths = max 0 (state.population - state.consumed // 20)
        avgDeaths = (toFloat (state.year - 1) * state.avgDeaths + 
                     toFloat (state.deaths * 100) / toFloat state.population) / toFloat state.year
    in
        Ok 
            { state
            | rnd = rnd1
            , births = births
            , deaths = deaths
            , totalDeaths = state.totalDeaths + deaths
            , avgDeaths = avgDeaths
            , population = state.population + births - deaths
            , impeached = (toFloat deaths) > 0.45 * (toFloat state.population)
            }

plague : State -> Result String State
plague state =
    let
        (luck, rnd1) = Random.step (Random.float 0 1) state.rnd
        strikes = luck < 0.15
        population = if strikes then state.population // 2 else state.population
    in
        Ok 
            { state
            | rnd = rnd1
            , population = population
            , plague = strikes
            }

endYear : State -> Result String State
endYear state =
    let
        (price, rnd1) = Random.step (Random.int 17 26) state.rnd
    in
        Ok 
            { state
            | year = state.year + 1
            , rnd = rnd1
            , price = price
            }

view : Model -> Html Msg
view model =
    case model of
        Playing form state ->
            viewPlaying form state
        Finished state ->
            viewFinished state

viewPlaying : Form -> State -> Html Msg
viewPlaying form state =
    div []
        [ viewState state
        , viewForm form
        ]

viewForm : Form -> Html Msg
viewForm form =
    div []
        [ div []
            [ labelledInput "Buy" form.buy Buy
            , labelledInput "Sell" form.sell Sell
            , labelledInput "Feed" form.feed Feed
            , labelledInput "Plant" form.plant Plant
            ]
        , button [ onClick DoIt ] [ text "Let it be done" ]
        , div []
            [ p [] [ text <| Maybe.withDefault "" form.error ]
            ]
        ]

labelledInput : String -> String -> (String -> Msg) -> Html Msg
labelledInput lbl val msg =
    label []
        [ text lbl
        , input [ type_ "text", placeholder lbl, value val, onInput msg ] []
        ]

viewState : State -> Html Msg
viewState state =
    div []
        [ p []
            [ text "Hammurabi: I beg to report to you,"
            , br [] []
            , text <| "In year " ++ String.fromInt state.year ++ ", "
                    ++ String.fromInt state.deaths ++ " people starved, " 
                    ++ String.fromInt state.births ++ " came to the city." 
            , br [] []
            , if state.plague then
                text <| "A horrible plague struck! Half the people died." 
              else 
                text ""
            , if state.plague then br [] [] else text ""
            , text <| "The city population is now " ++ String.fromInt state.population ++ "."
            , br [] []
            , text <| "The city now owns " ++ String.fromInt state.acres ++ " acres."
            , br [] []
            , text <| "You harvested " ++ String.fromInt state.yield ++ " bushels per acre."
            , br [] []
            , text <| "Rats ate " ++ String.fromInt state.devoured ++ " bushels."
            , br [] []
            , text <| "You now have " ++ String.fromInt state.bushels ++ " bushels in store."
            ]
        , p [] 
            [ text <| "Land is trading at " ++ String.fromInt state.price ++ " bushels per acre." 
            ]
        ]

viewFinished : State -> Html Msg
viewFinished state =
    div []
        [ div []
            [ viewFinalReport state
            , viewScore state
            ]
        , button [ onClick Restart ] [ text "Play again" ]
        ]

viewFinalReport : State -> Html Msg
viewFinalReport state =
    if state.impeached then
        p [] 
            [ text <| "You starved " ++ String.fromInt state.deaths ++ " people in one year!!!"
            ]
    else
        let
            app = toFloat state.acres / toFloat state.population
        in
            p []
                [ text <| "In your ten-year term of office, " ++ String.fromFloat state.avgDeaths ++ 
                    " percent of the population starved per year on average, i.e. a total of " ++ String.fromInt state.totalDeaths ++
                    " people died."
                , br [] []
                , text <| "You started with 10 acres per person and ended with " ++ String.fromFloat app ++ " acres per person."
                ]

viewScore : State -> Html Msg
viewScore state =
    case score state of
        Terrible ->
            terribleScore
        Awful ->
            awfulScore
        Good ->
            goodScore state
        Excellent ->
            excellentScore

terribleScore : Html Msg
terribleScore =
    p [] 
        [ text "Due to this extreme mismanagement you have not only been impeached "
        , text "and thrown out of office but you have also been declared national fink!!!"
        ]

awfulScore : Html Msg
awfulScore =
    p [] 
        [ text "Your heavy-handed performance smacks of Nero and Ivan IV. "
        , br [] []
        , text "The people (remaining) find you an unpleasant ruler, and, frankly, hate your guts!!"
        ]

goodScore : State -> Html Msg
goodScore state =
    let
        (luck, rnd1) = Random.step (Random.float 0 1) state.rnd
        haters = round (toFloat state.population * 0.8 * luck)
    in
        p [] 
            [ text "Your performance could be somewhat better, but really wasn't too bad at all."
            , br [] []
            , text <| String.fromInt haters ++ " people dearly want to see you assasinated but we all have our trivial problems."
            ]

excellentScore : Html Msg
excellentScore =
    p [] 
        [ text "A fantastic performance!!! Charlemagne, Disraeli, and Jefferson combined could not have done better"
        ]

type Score 
    = Terrible
    | Awful
    | Good
    | Excellent

score : State -> Score
score state =
    let
        app = toFloat state.acres / toFloat state.population
    in
        if state.avgDeaths > 33 || app < 7  || state.impeached then
            Terrible
        else if state.avgDeaths > 10 || app < 9 then
            Awful
        else if state.avgDeaths > 3 || app < 10 then
            Good
        else
            Excellent

main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }