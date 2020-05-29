module StatisticsBench exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Statistics as Stat


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    let
        testdata =
            List.range 0 100 |> List.map toFloat |> List.map ((*) 123.1234567)
    in
    describe "Statistics"
        [ describe "average"
            [ benchmark "avg" <|
                \_ -> testdata |> Stat.avg
            , benchmark "median" <|
                \_ -> testdata |> Stat.minmax
            , benchmark "minmax" <|
                \_ -> testdata |> Stat.minmax
            ]
        , describe "percentile"
            [ benchmark "0%" <|
                \_ -> testdata |> Stat.percentile 0
            , benchmark "50%" <|
                \_ -> testdata |> Stat.percentile 0.5
            , benchmark "100%" <|
                \_ -> testdata |> Stat.percentile 1
            ]
        , Benchmark.compare "middle element"
            "percentile 50%"
            (\_ -> testdata |> Stat.percentile 0.5)
            "median"
            (\_ -> testdata |> Stat.median)
        , Benchmark.compare "first element"
            "percentile 0%"
            (\_ -> testdata |> Stat.percentile 0.5)
            "List.head"
            (\_ -> testdata |> List.head)
        ]
