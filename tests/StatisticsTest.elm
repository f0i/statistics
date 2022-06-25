module StatisticsTest exposing (..)

import Dict
import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, float, intRange, list)
import List.Nonempty
import List.Nonempty.Statistics as StatNe
import List.Statistics as Stat
import Test exposing (..)


{-| Int fuzzer for well defined range of Int
as defined in [package.elm-lang.org/…/Basics#Int](https://package.elm-lang.org/packages/elm/core/latest/Basics#Int)
-}
int : Fuzzer Int
int =
    intRange (-2 ^ 31) (2 ^ 31 - 1)


suite : Test
suite =
    let
        float9 =
            [ 13.0, 13, 13, 13, 14, 14, 16, 18, 21 ]

        float9nonempty =
            List.Nonempty.fromList float9
                |> Maybe.withDefault (List.Nonempty.singleton 0)

        float4 =
            [ 1.0, 2, 4, 7 ]

        int9 =
            [ 13, 13, 13, 13, 14, 14, 16, 18, 21 ]

        int9nonempty =
            List.Nonempty.fromList int9
                |> Maybe.withDefault (List.Nonempty.singleton 0)

        int4 =
            [ 1, 2, 4, 7 ]
    in
    describe "List.Statistics"
        [ describe "avg/mean"
            [ test "9 Floats" <|
                \_ ->
                    float9
                        |> Stat.avg
                        |> Expect.equal (Just 15)
            , test "4 Floats" <|
                \_ ->
                    float4
                        |> Stat.mean
                        |> Expect.equal (Just 3.5)
            , test "empty" <|
                \_ ->
                    []
                        |> Stat.avg
                        |> Expect.equal Nothing
            ]
        , describe "avgInt/meanInt"
            [ test "9 Ints" <|
                \_ ->
                    int9
                        |> Stat.avgInt
                        |> Expect.equal (Just 15)
            , test "4 Ints" <|
                \_ ->
                    int4
                        |> Stat.meanInt
                        |> Expect.equal (Just 3)
            , test "empty" <|
                \_ ->
                    []
                        |> Stat.avgInt
                        |> Expect.equal Nothing
            ]
        , describe "fuzz avg"
            [ fuzz (list int) "avgInt is equal to truncated avg" <|
                \randomList ->
                    randomList
                        |> List.map toFloat
                        |> Stat.avg
                        |> Maybe.map truncate
                        |> Expect.equal (randomList |> Stat.avgInt)
            ]
        , describe "median"
            [ test "9 FLoats" <|
                \_ ->
                    float9
                        |> Stat.median
                        |> Expect.equal (Just 14)
            , test "4 Floats" <|
                \_ ->
                    float4
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
                    float9
                        |> Stat.minmax
                        |> Expect.equal (Just ( 13, 21 ))
            , test "4 Floats" <|
                \_ ->
                    float4
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
                    float9
                        |> Stat.occurrences
                        |> Dict.toList
                        |> Expect.equal [ ( 13, 4 ), ( 14, 2 ), ( 16, 1 ), ( 18, 1 ), ( 21, 1 ) ]
            , test "4 Floats" <|
                \_ ->
                    float4
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
                    float9
                        |> Stat.percentile 0.5
                        |> Expect.equal (Just 14)
            , test "90%" <|
                \_ ->
                    float9
                        |> Stat.percentile 0.9
                        |> Expect.equal (Just 18.6)
            , test "42.125%" <|
                \_ ->
                    float9
                        |> Stat.percentile 0.42125
                        |> Expect.equal (Just 13.37)
            , test "0%" <|
                \_ ->
                    float9
                        |> Stat.percentile 0
                        |> Expect.equal (Just 13)
            , test "100%" <|
                \_ ->
                    float9
                        |> Stat.percentile 1
                        |> Expect.equal (Just 21)
            ]
        , describe "fuzz percentile"
            [ fuzz (list float) "50% equals median" <|
                \randomList ->
                    randomList
                        |> Stat.percentile 0.5
                        |> Expect.equal (randomList |> Stat.median)
            , fuzz (list int) "50% equals medianInt" <|
                \randomList ->
                    randomList
                        |> Stat.percentileInt 0.5
                        |> Expect.equal (randomList |> Stat.medianInt)
            , fuzz (list float) "0% equals List.head" <|
                \randomList ->
                    randomList
                        |> Stat.percentile 0
                        |> Expect.equal (randomList |> List.head)
            ]
        , describe "percentiles"
            [ test "0, 25, 50, 75 and 100%" <|
                \_ ->
                    float9
                        |> Stat.percentiles [ 0, 0.25, 0.5, 0.75, 1 ]
                        |> Expect.equal (Just [ 13, 13, 14, 16, 21 ])
            , test "Nonempty 0, 25, 50, 75 and 100%" <|
                \_ ->
                    float9nonempty
                        |> StatNe.percentiles [ 0, 0.25, 0.5, 0.75, 1 ]
                        |> Expect.equal [ 13, 13, 14, 16, 21 ]
            ]
        , describe "percentilesInt"
            [ test "0, 25, 50, 75 and 100%" <|
                \_ ->
                    int9
                        |> Stat.percentilesInt [ 0, 0.25, 0.5, 0.75, 1 ]
                        |> Expect.equal (Just [ 13, 13, 14, 16, 21 ])
            , test "0, 25, 50, 75 and 100%" <|
                \_ ->
                    int9nonempty
                        |> StatNe.percentilesInt [ 0, 0.25, 0.5, 0.75, 1 ]
                        |> Expect.equal [ 13, 13, 14, 16, 21 ]
            ]
        , describe "variance"
            [ test "9 FLoats" <|
                \_ ->
                    float9
                        |> Stat.variance
                        |> Maybe.withDefault 0
                        |> Expect.within (Absolute 0.0001) 7.1111
            , test "4 Floats" <|
                \_ ->
                    float4
                        |> Stat.variance
                        |> Maybe.withDefault 0
                        |> Expect.within (Absolute 0.0001) 5.25
            , test "empty" <|
                \_ ->
                    []
                        |> Stat.varianceInt
                        |> Expect.equal Nothing
            ]
        , describe "variance Int"
            [ test "9 Ints" <|
                \_ ->
                    int9
                        |> Stat.varianceInt
                        |> Expect.equal (Just 7)
            , test "4 Ints" <|
                \_ ->
                    int4
                        |> Stat.varianceInt
                        |> Maybe.withDefault 0
                        |> toFloat
                        |> Expect.within (Absolute 1) 5
            , test "empty" <|
                \_ ->
                    []
                        |> Stat.variance
                        |> Expect.equal Nothing
            ]
        , describe "standard deviation"
            [ test "9 FLoats" <|
                \_ ->
                    float9
                        |> Stat.stdDeviation
                        |> Maybe.withDefault 0
                        |> Expect.within (Absolute 0.0001) 2.6666
            , test "4 Floats" <|
                \_ ->
                    float4
                        |> Stat.stdDeviation
                        |> Maybe.withDefault 0
                        |> Expect.within (Absolute 0.0001) 2.2913
            , test "empty" <|
                \_ ->
                    []
                        |> Stat.stdDeviation
                        |> Expect.equal Nothing
            ]
        , describe "standard deviation Int"
            [ test "9 Ints" <|
                \_ ->
                    int9
                        |> Stat.stdDeviationInt
                        |> Expect.equal (Just 3)
            , test "4 Ints" <|
                \_ ->
                    int4
                        |> Stat.stdDeviationInt
                        |> Expect.equal (Just 2)
            , test "empty" <|
                \_ ->
                    []
                        |> Stat.stdDeviation
                        |> Expect.equal Nothing
            ]
        ]
