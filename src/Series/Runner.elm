module Series.Runner exposing (..)

import Dict exposing (Dict)
import Json.Encode exposing (Value)
import Series.LowLevel as LowLevel
import Task exposing (Task)


type Series comparable
    = Series String (Dict comparable Comparison)


type Comparison
    = Comparison LowLevel.Benchmark (List LowLevel.Benchmark)


baseline : Comparison -> LowLevel.Benchmark
baseline (Comparison baseline _) =
    baseline


contenders : Comparison -> List LowLevel.Benchmark
contenders (Comparison _ contenders) =
    contenders


compare : List LowLevel.Benchmark -> Comparison
compare benches =
    case benches of
        x :: y :: xs ->
            Comparison x (y :: xs)

        _ ->
            Debug.crash "you need to provide at least two benchmarks to compare"


series : String -> (comparable -> Comparison) -> List comparable -> Series comparable
series name toBenchmark cases =
    List.map (\aCase -> ( aCase, toBenchmark aCase )) cases
        |> Dict.fromList
        |> Series name


maybeTaskList :
    (a -> Maybe (Task e a))
    -> List a
    -> Maybe (Task e (List a))
maybeTaskList toMaybeTask list =
    let
        tasks =
            List.map toMaybeTask list
    in
    if List.all ((==) Nothing) tasks then
        Nothing
    else
        List.map2
            (\maybeTask original ->
                Maybe.withDefault (Task.succeed original) maybeTask
            )
            tasks
            list
            |> Task.sequence
            |> Just


step : Series comparable -> Maybe (Task Never (Series comparable))
step (Series name entries) =
    let
        tasks =
            Dict.toList entries
                |> List.filterMap
                    (\( key, entry ) ->
                        entry
                            |> stepCompare
                            |> Maybe.map (Task.map ((,) key))
                    )
    in
    case tasks of
        [] ->
            Nothing

        _ ->
            List.foldr
                (Task.map2
                    (\( key, value ) dict ->
                        Dict.insert key value dict
                    )
                )
                (Task.succeed entries)
                tasks
                |> Task.map (Series name)
                |> Just


stepCompare : Comparison -> Maybe (Task Never Comparison)
stepCompare (Comparison baseline cases) =
    case ( LowLevel.step baseline, maybeTaskList LowLevel.step cases ) of
        ( Nothing, Nothing ) ->
            Nothing

        ( left, right ) ->
            Task.map2 Comparison
                (Maybe.withDefault (Task.succeed baseline) left)
                (Maybe.withDefault (Task.succeed cases) right)
                |> Just


type Progress
    = Sizing
    | InProgress ProgressStats
    | Invalid


type alias ProgressStats =
    { current : Float, total : Float, errors : Int }


combine :
    Progress
    -> Progress
    -> Progress
combine left right =
    case ( left, right ) of
        ( InProgress a, InProgress b ) ->
            InProgress
                { current = a.current + b.current
                , total = a.total + b.total
                , errors = a.errors + b.errors
                }

        ( Invalid, _ ) ->
            Invalid

        ( _, Invalid ) ->
            Invalid

        _ ->
            Sizing


stats : Series comparable -> Progress
stats (Series _ entries) =
    Dict.foldl
        (\_ report acc_ ->
            case acc_ of
                Nothing ->
                    Just <| comparisonStats report

                Just acc ->
                    Just <| combine acc (comparisonStats report)
        )
        Nothing
        entries
        |> Maybe.withDefault Invalid


comparisonStats : Comparison -> Progress
comparisonStats (Comparison baseline cases) =
    List.foldr combine
        (LowLevel.status baseline |> statusToStats)
        (List.map (LowLevel.status >> statusToStats) cases)


statusToStats : LowLevel.Status -> Progress
statusToStats status =
    case status of
        LowLevel.ToSize total ->
            Sizing

        LowLevel.Pending total _ samples ->
            InProgress
                { current = min total (List.sum samples)
                , total = total
                , errors = 0
                }

        LowLevel.Failure _ ->
            InProgress { current = 0, total = 0, errors = 1 }

        LowLevel.Success _ samples ->
            let
                total : Float
                total =
                    List.sum samples
            in
            InProgress { current = total, total = total, errors = 0 }


encodeSeries : (comparable -> Value) -> Series comparable -> Maybe Value
encodeSeries encodeKey (Series name variations) =
    case List.filterMap (encodeComparison encodeKey) (Dict.toList variations) of
        [] ->
            Nothing

        encodedEntries ->
            [ ( "name", Json.Encode.string name )
            , ( "variations", Json.Encode.list encodedEntries )
            ]
                |> Json.Encode.object
                |> Just


encodeComparison : (a -> Value) -> ( a, Comparison ) -> Maybe Value
encodeComparison encodeKey ( key, Comparison baseline cases ) =
    Maybe.map
        (\encodedBaseline ->
            [ ( "variation", encodeKey key )
            , ( "baseline", encodedBaseline )
            , ( "cases", Json.Encode.list <| List.filterMap encodeBench cases )
            ]
                |> Json.Encode.object
        )
        (encodeBench baseline)


encodeBench : LowLevel.Benchmark -> Maybe Value
encodeBench bench =
    bench
        |> LowLevel.status
        |> encodeStats
        |> Maybe.map
            (\attrs ->
                ([ ( "name", LowLevel.name bench |> Json.Encode.string )
                 ]
                    ++ attrs
                )
                    |> Json.Encode.object
            )


encodeStats : LowLevel.Status -> Maybe (List ( String, Value ))
encodeStats status =
    case status of
        LowLevel.ToSize _ ->
            Nothing

        LowLevel.Pending _ _ _ ->
            Nothing

        LowLevel.Failure e ->
            [ ( "error", Json.Encode.string <| toString e ) ]
                |> Just

        LowLevel.Success runs samples ->
            [ ( "sampleSize", Json.Encode.int runs )
            , ( "samples", Json.Encode.list (List.map Json.Encode.float samples) )
            ]
                |> Just
