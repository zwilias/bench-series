module Series.Runner
    exposing
        ( Comparison
        , Progress(..)
        , ProgressStats
        , Series
        , baseline
        , compare
        , contenders
        , encode
        , entries
        , name
        , series
        , stats
        , step
        )

{-| Provides structural primitives for running a series of benchmarks.


# Creating structure

@docs Series, Comparison
@docs series, compare


# Inspecting structure

@docs name, entries
@docs baseline, contenders


# How are things doing?

@docs Progress, ProgressStats
@docs stats


# Other useful things

@docs step
@docs encode

-}

import Dict exposing (Dict)
import Json.Encode exposing (Value)
import Series.LowLevel as LowLevel
import Task exposing (Task)


{-| A series represents .. uhm, a series of comparisons.

It's useful for checking how a thing behaves over a range of inputs.

-}
type Series comparable
    = Series String (Dict comparable Comparison)


{-| A series has a name. For reasons. Maybe it shouldn't, but it does.
-}
name : Series comparable -> String
name (Series name _) =
    name


{-| A series has a dictionary of entries, for each input, there's a single
`Comparison`.
-}
entries : Series comparable -> Dict comparable Comparison
entries (Series _ entries) =
    entries


{-| A Comparison compares performance of a list of contenders versus a baseline.
-}
type Comparison
    = Comparison LowLevel.Benchmark (List LowLevel.Benchmark)


{-| Gives you the baseline of a `Comparison`.
-}
baseline : Comparison -> LowLevel.Benchmark
baseline (Comparison baseline _) =
    baseline


{-| Gives you the contentders of a `Comparison`.
-}
contenders : Comparison -> List LowLevel.Benchmark
contenders (Comparison _ contenders) =
    contenders


{-| Compare a bunch of benchmarks. The first entry is the baseline, the rest are
contenders. If you don't give this at least 2 benchmarks, it'll explode.

I'm not really expecting anyone to put benchmarks in production, tho.

-}
compare : List LowLevel.Benchmark -> Comparison
compare benches =
    case benches of
        x :: y :: xs ->
            Comparison x (y :: xs)

        _ ->
            Debug.crash "you need to provide at least two benchmarks to compare"


{-| Create a series of benchmarks. It takes a name, a function to create a
comparison for an input, and a bunch of inputs.
-}
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


{-| Tries to `step` a series. Same rules as for `LowLevel.Benchmark` apply.

If the benchmark is "done" (or completely errored out), this will return
`Nothing`. If there is anything left to do, you'll get a `Task` to `perform`.

-}
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


{-| A Series can either be sizing, in progress (with some statistics), or
invalid, somehow.

For an example of an invalid benchmark, consider a series with no inputs.

-}
type Progress
    = Sizing
    | InProgress ProgressStats
    | Invalid


{-| Statistics. How much time we've spent, how much we'll need to spend, and how
many errors we've had.
-}
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


{-| Extract statistics from a Series.
-}
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


{-| Encode a benchmark. Json is cool.
-}
encode : (comparable -> Value) -> Series comparable -> Maybe Value
encode encodeKey (Series name variations) =
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
