module StatisticsTest exposing (..)

import Dict exposing (Dict)
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, int, list, string)
import Statistics as Stat
import Test exposing (..)


testdata9 =
    [ 13, 13, 13, 13, 14, 14, 16, 18, 21 ]


testdata4 =
    [ 1, 2, 4, 7 ]


suite : Test
suite =
    describe "Statistics"
        [ describe "avg/mean"
            [ test "9 Floats" <|
                \_ ->
                    testdata9
                        |> Stat.avg
                        |> Expect.equal (Just 15)
            , test "4 Floats" <|
                \_ ->
                    testdata4
                        |> Stat.mean
                        |> Expect.equal (Just 3.5)
            , test "empty" <|
                \_ ->
                    []
                        |> Stat.avg
                        |> Expect.equal Nothing
            ]
        , describe "median"
            [ test "9 FLoats" <|
                \_ ->
                    testdata9
                        |> Stat.median
                        |> Expect.equal (Just 14)
            , test "4 Floats" <|
                \_ ->
                    testdata4
                        |> Stat.median
                        |> Expect.equal (Just 3)
            , test "empty" <|
                \_ ->
                    []
                        |> Stat.median
                        |> Expect.equal Nothing
            ]
        , describe "minmax"
            [ test "9 FLoats" <|
                \_ ->
                    testdata9
                        |> Stat.minmax
                        |> Expect.equal (Just ( 13, 21 ))
            , test "4 Floats" <|
                \_ ->
                    testdata4
                        |> Stat.minmax
                        |> Expect.equal (Just ( 1, 7 ))
            , test "empty" <|
                \_ ->
                    []
                        |> Stat.minmax
                        |> Expect.equal Nothing
            ]
        , describe "count"
            [ test "9 FLoats" <|
                \_ ->
                    testdata9
                        |> Stat.occurrences
                        |> Dict.toList
                        |> Expect.equal [ ( 13, 4 ), ( 14, 2 ), ( 16, 1 ), ( 18, 1 ), ( 21, 1 ) ]
            , test "4 Floats" <|
                \_ ->
                    testdata4
                        |> Stat.minmax
                        |> Expect.equal (Just ( 1, 7 ))
            , test "empty" <|
                \_ ->
                    []
                        |> Stat.minmax
                        |> Expect.equal Nothing
            ]
        , describe "percentile"
            [ test "50%" <|
                \_ ->
                    testdata9
                        |> Stat.percentile 0.5
                        |> Expect.equal (Just 14)
            , test "90%" <|
                \_ ->
                    testdata9
                        |> Stat.percentile 0.9
                        |> Expect.equal (Just 18.6)
            , test "42.125%" <|
                \_ ->
                    testdata9
                        |> Stat.percentile 0.42125
                        |> Expect.equal (Just 13.37)
            , test "0%" <|
                \_ ->
                    testdata9
                        |> Stat.percentile 0
                        |> Expect.equal (Just 13)
            , test "100%" <|
                \_ ->
                    testdata9
                        |> Stat.percentile 1
                        |> Expect.equal (Just 21)
            ]
        , describe "standard deviation"
            [ test "9 FLoats" <|
                \_ ->
                    testdata9
                        |> Stat.stdDeviation
                        |> Maybe.withDefault 0
                        |> Expect.within (Absolute 0.0001) 2.6666
            , test "4 Floats" <|
                \_ ->
                    testdata4
                        |> Stat.stdDeviation
                        |> Maybe.withDefault 0
                        |> Expect.within (Absolute 0.0001) 2.2913
            , test "empty" <|
                \_ ->
                    []
                        |> Stat.stdDeviation
                        |> Expect.equal Nothing
            ]
        ]
