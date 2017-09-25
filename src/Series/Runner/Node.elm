module Series.Runner.Node exposing (SeriesProgram, program)

import Console
import Dict exposing (Dict)
import Json.Encode exposing (Value)
import Process
import Series.LowLevel as LowLevel
import Series.Runner as Runner exposing (Series(..))
import Task exposing (Task)


program :
    (Value -> Cmd (Msg comparable))
    -> (comparable -> Value)
    -> Series comparable
    -> SeriesProgram comparable
program emit encodeKey benchmarks =
    Platform.program
        { init = init emit encodeKey benchmarks
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model comparable =
    { benchmarks : Series comparable
    , encodeKey : comparable -> Value
    , emit : Value -> Cmd (Msg comparable)
    }


type Msg comparable
    = Update (Series comparable)


type alias SeriesProgram a =
    Program Never (Model a) (Msg a)


init :
    (Value -> Cmd (Msg comparable))
    -> (comparable -> Value)
    -> Series comparable
    -> ( Model comparable, Cmd (Msg comparable) )
init emit encodeKey benchmarks =
    { emit = emit
    , encodeKey = encodeKey
    , benchmarks = benchmarks
    }
        ! [ step benchmarks |> Maybe.withDefault Cmd.none
          , emit <| running benchmarks
          , emit <| start benchmarks
          ]


update : Msg comparable -> Model comparable -> ( Model comparable, Cmd (Msg comparable) )
update (Update benchmark) ({ emit, encodeKey } as model) =
    case step benchmark of
        Just cmd ->
            { model | benchmarks = benchmark } ! [ cmd, emit <| running benchmark ]

        Nothing ->
            { model | benchmarks = benchmark } ! [ emit <| done encodeKey benchmark ]


breakForRender : Task x a -> Task x a
breakForRender task =
    Process.sleep 0 |> Task.andThen (always task)


step : Series comparable -> Maybe (Cmd (Msg comparable))
step benchmark =
    Runner.step benchmark
        |> Maybe.map breakForRender
        |> Maybe.map (Task.perform Update)


progressBar : Int -> Runner.Progress -> String
progressBar width progress =
    case progress of
        Runner.Sizing ->
            "Sizing..."

        Runner.Invalid ->
            "Invalid benchmark structure."

        Runner.InProgress { current, total, errors } ->
            let
                done : Int
                done =
                    (current * 8 * toFloat width)
                        / total
                        |> floor

                toGo : Int
                toGo =
                    width - ceiling (toFloat done / 8)

                percentDone : Int
                percentDone =
                    (current * toFloat 100)
                        / total
                        |> floor

                error : String
                error =
                    if errors > 0 then
                        " " ++ toString errors ++ " errors"
                    else
                        ""
            in
            [ "▕"
            , String.repeat (done // 8) "█"
            , block (done % 8)
            , String.repeat toGo "·"
            , "▏"
            , String.padLeft 4 ' ' (toString percentDone ++ "%")
            , error
            ]
                |> String.concat


block : Int -> String
block i =
    case i of
        1 ->
            "▏"

        2 ->
            "▎"

        3 ->
            "▍"

        4 ->
            "▌"

        5 ->
            "▋"

        6 ->
            "▊"

        7 ->
            "▉"

        8 ->
            "█"

        _ ->
            ""


start : Series comparable -> Value
start report =
    Json.Encode.object
        [ ( "type", Json.Encode.string "start" )
        , ( "data"
          , Json.Encode.string <|
                Console.bold "\n⏱  Running benchmarks...\n\n"
                    ++ makePrettyIntro report
                    ++ "\n"
          )
        ]


running : Series comparable -> Value
running report =
    Json.Encode.object
        [ ( "type", Json.Encode.string "running" )
        , ( "data"
          , Json.Encode.string <| "\x0D" ++ progressBar 72 (Runner.stats report)
          )
        ]


done : (comparable -> Value) -> Series comparable -> Value
done encodeKey report =
    Json.Encode.object
        [ ( "type", Json.Encode.string "done" )
        , ( "msg", Json.Encode.string <| "\x0D" ++ progressBar 72 (Runner.stats report) )
        , ( "data", Runner.encodeSeries encodeKey report |> Maybe.withDefault (Json.Encode.object []) )
        ]


indent : Int -> String -> String
indent n =
    (++) (String.repeat n " ")


makePrettyIntro : Series comparable -> String
makePrettyIntro =
    makePrettyIntroLines >> String.join "\n"


makePrettyIntroLines : Series comparable -> List String
makePrettyIntroLines (Series name variations) =
    let
        firstVariation : Maybe Runner.Comparison
        firstVariation =
            Dict.toList variations
                |> List.head
                |> Maybe.map Tuple.second
    in
    Console.bold name
        :: indent 2 (Console.dark "→ Variations: " ++ gatherVariations variations)
        :: indent 2 (Console.dark "→ Baseline: " ++ (Maybe.map (LowLevel.name << Runner.baseline) firstVariation |> Maybe.withDefault "(unknown)"))
        :: indent 2 (Console.dark "→ Contenders: ")
        :: gatherContenders (Maybe.map Runner.contenders firstVariation |> Maybe.withDefault [])


gatherVariations : Dict comparable v -> String
gatherVariations variations =
    Dict.keys variations
        |> List.sort
        |> List.map toString
        |> String.join ", "


gatherContenders : List LowLevel.Benchmark -> List String
gatherContenders =
    List.map (LowLevel.name >> (++) "▸ " >> indent 4)
