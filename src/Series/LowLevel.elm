module Series.LowLevel
    exposing
        ( Benchmark
        , Error
        , Status(..)
        , benchmark
        , benchmark1
        , benchmark2
        , benchmark3
        , benchmark4
        , benchmark5
        , benchmark6
        , benchmark7
        , benchmark8
        , name
        , retime
        , status
        , step
        )

{-| Somewhat higher level wrappers around `Benchmark.LowLevel`.


# Constructing benchmarks

@docs Benchmark
@docs benchmark, benchmark1, benchmark2, benchmark3, benchmark4, benchmark5
@docs benchmark6, benchmark7, benchmark8, retime


# Inspecting benchmarks

@docs Status, Error, name, status, step

-}

import Benchmark.LowLevel as LowLevel
import Task exposing (Task)
import Time exposing (Time)


{-| Alias for [Benchmark.LowLevel.Error](http://package.elm-lang.org/packages/BrianHicks/elm-benchmark/1.0.2/Benchmark-LowLevel#Error)
-}
type alias Error =
    LowLevel.Error


{-| Represents the current status of a Benchmark
-}
type Status
    = ToSize Time
    | Pending Time Int (List Time)
    | Failure Error
    | Success Int (List Time)


{-| A benchmark.
-}
type Benchmark
    = Benchmark String LowLevel.Operation Status


{-| Retrieve the current status of a Benchmark
-}
status : Benchmark -> Status
status (Benchmark _ _ status) =
    status


{-| Benchmarks have a name. Makes them easy to identify.
-}
name : Benchmark -> String
name (Benchmark name _ _) =
    name


makeBenchmark : String -> LowLevel.Operation -> Benchmark
makeBenchmark name operation =
    Benchmark name operation <| ToSize <| 5 * Time.second


{-| Want your Benchmark to run for something other than 5 seconds? I'm your tool.
-}
retime : Time -> Benchmark -> Benchmark
retime time (Benchmark name operation _) =
    Benchmark name operation (ToSize time)


{-| Benchmark a lambda.
-}
benchmark : String -> (() -> a) -> Benchmark
benchmark name fn =
    makeBenchmark name (LowLevel.operation fn)


{-| Benchmark a function that takes a single argument.
-}
benchmark1 : String -> (a -> b) -> a -> Benchmark
benchmark1 name fn a =
    makeBenchmark name (LowLevel.operation <| \_ -> fn a)


{-| Benchmark a function that takes 2 arguments.
-}
benchmark2 : String -> (a -> b -> c) -> a -> b -> Benchmark
benchmark2 name fn a b =
    makeBenchmark name (LowLevel.operation <| \_ -> fn a b)


{-| Benchmark a function that takes 3 arguments.
-}
benchmark3 : String -> (a -> b -> c -> d) -> a -> b -> c -> Benchmark
benchmark3 name fn a b c =
    makeBenchmark name (LowLevel.operation <| \_ -> fn a b c)


{-| Benchmark a function that takes 4 arguments.
-}
benchmark4 : String -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> Benchmark
benchmark4 name fn a b c d =
    makeBenchmark name (LowLevel.operation <| \_ -> fn a b c d)


{-| Benchmark a function that takes 5 arguments.
-}
benchmark5 : String -> (a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> Benchmark
benchmark5 name fn a b c d e =
    makeBenchmark name (LowLevel.operation <| \_ -> fn a b c d e)


{-| Benchmark a function that takes 6 arguments.

(Don't make functions that take that many arguments.)

-}
benchmark6 : String -> (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> Benchmark
benchmark6 name fn a b c d e f =
    makeBenchmark name (LowLevel.operation <| \_ -> fn a b c d e f)


{-| Benchmark a function that takes 7 arguments.

(Don't make functions that take that many arguments.)

-}
benchmark7 : String -> (a -> b -> c -> d -> e -> f -> g -> h) -> a -> b -> c -> d -> e -> f -> g -> Benchmark
benchmark7 name fn a b c d e f g =
    makeBenchmark name (LowLevel.operation <| \_ -> fn a b c d e f g)


{-| Benchmark a function that takes 8 arguments.

(I don't like your API.)

-}
benchmark8 : String -> (a -> b -> c -> d -> e -> f -> g -> h -> i) -> a -> b -> c -> d -> e -> f -> g -> h -> Benchmark
benchmark8 name fn a b c d e f g h =
    makeBenchmark name (LowLevel.operation <| \_ -> fn a b c d e f g h)


{-| If there is another action to be taken for your benchmark, this will return
a Task to do that. If the benchmark is done or errored out, This will give you
`Nothing` at all.
-}
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
