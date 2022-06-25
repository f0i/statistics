module StatisticsBench exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import List.Statistics as Stat
import Stat as ElmStat
import Statistics as ElmVisual
import Trend.Math as ElmTrend


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    let
        count =
            100

        testdata =
            List.range 0 count |> List.map toFloat |> List.map ((*) 123.1234567)

        info =
            "Testdata is a List of " ++ String.fromInt count ++ " Floats"
    in
    describe "List.Statistics"
        [ describe "internal comparison"
            [ Benchmark.compare ("average | " ++ info)
                "avg/mean"
                (\_ -> testdata |> Stat.avg)
                "median"
                (\_ -> testdata |> Stat.median)
            , Benchmark.compare "minmax"
                "minmax"
                (\_ -> testdata |> Stat.minmax)
                "min, max"
                (\_ -> ( List.minimum testdata, List.maximum testdata ))
            , Benchmark.compare "percentile"
                "0%"
                (\_ -> testdata |> Stat.percentile 0)
                "100%"
                (\_ -> testdata |> Stat.percentile 1)
            , Benchmark.compare "middle element"
                "percentile 50%"
                (\_ -> testdata |> Stat.percentile 0.5)
                "median"
                (\_ -> testdata |> Stat.median)
            , Benchmark.compare "first element"
                "percentile 0%"
                (\_ -> testdata |> Stat.percentile 0.0)
                "List.head"
                (\_ -> testdata |> List.head)
            , Benchmark.compare "percentiles 10%, 50%, 90%"
                "percentiles"
                (\_ -> testdata |> Stat.percentiles [ 0.1, 0.5, 0.9 ])
                "3x percentile"
                (\_ ->
                    [ testdata |> Stat.percentile 0.1
                    , testdata |> Stat.percentile 0.5
                    , testdata |> Stat.percentile 0.9
                    ]
                )
            ]
        , describe "comparison to other libraries"
            [ Benchmark.compare "percentile vs. gampleman/elm-visualization"
                "percentile 50%"
                (\_ -> testdata |> Stat.percentile 0.5)
                "elm-visual… quantile 50%"
                (\_ -> testdata |> ElmVisual.quantile 0.5)
            , Benchmark.compare "mean vs. BrianHicks/elm-trend"
                "mean"
                (\_ -> testdata |> Stat.mean)
                "elm-trend mean"
                (\_ -> testdata |> ElmTrend.mean |> Result.toMaybe)
            , Benchmark.compare "mean vs. jxxcarlson/elm-stat"
                "mean"
                (\_ -> testdata |> Stat.mean)
                "elm-stat mean"
                (\_ -> testdata |> ElmStat.mean identity)
            , Benchmark.compare "stdDeviation vs. BrianHicks/elm-trend"
                "stdDeviation"
                (\_ -> testdata |> Stat.stdDeviation)
                "elm-trend stddev"
                (\_ -> testdata |> ElmTrend.stddev |> Result.toMaybe)
            , Benchmark.compare "stdDeviation vs. jxxcarlson/elm-stat"
                "stdDeviation"
                (\_ -> testdata |> Stat.stdDeviation)
                "elm-stat stdev"
                (\_ -> testdata |> ElmStat.stdev identity)
            ]
        ]
