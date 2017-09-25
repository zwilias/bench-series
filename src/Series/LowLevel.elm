module Series.LowLevel exposing (..)

import Benchmark.LowLevel as LowLevel
import Task exposing (Task)
import Time exposing (Time)


type alias Error =
    LowLevel.Error


type Status
    = ToSize Time
    | Pending Time Int (List Time)
    | Failure Error
    | Success Int (List Time)


type Benchmark
    = Benchmark String LowLevel.Operation Status


status : Benchmark -> Status
status (Benchmark _ _ status) =
    status


name : Benchmark -> String
name (Benchmark name _ _) =
    name


makeBenchmark : String -> LowLevel.Operation -> Benchmark
makeBenchmark name operation =
    Benchmark name operation <| ToSize <| 5 * Time.second


retime : Time -> Benchmark -> Benchmark
retime time (Benchmark name operation _) =
    Benchmark name operation (ToSize time)


benchmark : String -> (() -> a) -> Benchmark
benchmark name fn =
    makeBenchmark name (LowLevel.operation fn)


benchmark1 : String -> (a -> b) -> a -> Benchmark
benchmark1 name fn a =
    makeBenchmark name (LowLevel.operation <| \_ -> fn a)


benchmark2 : String -> (a -> b -> c) -> a -> b -> Benchmark
benchmark2 name fn a b =
    makeBenchmark name (LowLevel.operation <| \_ -> fn a b)


benchmark3 : String -> (a -> b -> c -> d) -> a -> b -> c -> Benchmark
benchmark3 name fn a b c =
    makeBenchmark name (LowLevel.operation <| \_ -> fn a b c)


benchmark4 : String -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> Benchmark
benchmark4 name fn a b c d =
    makeBenchmark name (LowLevel.operation <| \_ -> fn a b c d)


benchmark5 : String -> (a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> Benchmark
benchmark5 name fn a b c d e =
    makeBenchmark name (LowLevel.operation <| \_ -> fn a b c d e)


benchmark6 : String -> (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> Benchmark
benchmark6 name fn a b c d e f =
    makeBenchmark name (LowLevel.operation <| \_ -> fn a b c d e f)


benchmark7 : String -> (a -> b -> c -> d -> e -> f -> g -> h) -> a -> b -> c -> d -> e -> f -> g -> Benchmark
benchmark7 name fn a b c d e f g =
    makeBenchmark name (LowLevel.operation <| \_ -> fn a b c d e f g)


benchmark8 : String -> (a -> b -> c -> d -> e -> f -> g -> h -> i) -> a -> b -> c -> d -> e -> f -> g -> h -> Benchmark
benchmark8 name fn a b c d e f g h =
    makeBenchmark name (LowLevel.operation <| \_ -> fn a b c d e f g h)


step : Benchmark -> Maybe (Task Never Benchmark)
step (Benchmark name operation status) =
    case status of
        ToSize time ->
            findSampleSize operation
                |> Task.map standardizeSampleSize
                |> Task.map (\sampleSize -> Pending time sampleSize [])
                |> Task.map (Benchmark name operation)
                |> Task.onError (Failure >> Benchmark name operation >> Task.succeed)
                |> Just

        Pending target sampleSize samples ->
            if List.sum samples < target then
                LowLevel.sample sampleSize operation
                    |> Task.map (flip (::) samples >> Pending target sampleSize >> Benchmark name operation)
                    |> Task.onError (Failure >> Benchmark name operation >> Task.succeed)
                    |> Just
            else
                Success sampleSize samples
                    |> Benchmark name operation
                    |> Task.succeed
                    |> Just

        _ ->
            Nothing


findSampleSize : LowLevel.Operation -> Task Error Int
findSampleSize operation =
    let
        initialSampleSize =
            1

        minimumRuntime =
            100 * Time.millisecond

        sampleOf : Int -> Task Error Time
        sampleOf size =
            LowLevel.sample size operation
                |> Task.andThen (resample size)

        -- increase the sample size by powers of 10 until we meet the minimum runtime
        resample : Int -> Time -> Task Error Time
        resample size total =
            if total < minimumRuntime then
                sampleOf (size * 10)
            else
                total / toFloat size |> Task.succeed

        fit : Time -> Int
        fit single =
            minimumRuntime / single |> ceiling
    in
    sampleOf initialSampleSize |> Task.map fit


standardizeSampleSize : Int -> Int
standardizeSampleSize sampleSize =
    let
        helper : Int -> Int -> Int
        helper rough magnitude =
            if rough > 10 then
                helper (toFloat rough / 10 |> round) (magnitude * 10)
            else
                rough * magnitude
    in
    helper sampleSize 1
