module List.Nonempty.Statistics exposing
    ( avg, avgInt, mean, meanInt
    , median, medianInt, percentile, percentileInt
    , percentiles, percentilesInt
    , minimum, maximum, minmax
    , occurrences
    , variance, varianceInt
    , stdDeviation, stdDeviationInt
    , sum, product
    )

{-| Fast Statistics functions for elm


# Average

@docs avg, avgInt, mean, meanInt
@docs median, medianInt, percentile, percentileInt
@docs percentiles, percentilesInt


# Minimum / maximum

@docs minimum, maximum, minmax


# Distribution

@docs occurrences
@docs variance, varianceInt
@docs stdDeviation, stdDeviationInt


# Misc

@docs sum, product

-}

import Dict exposing (Dict)
import List exposing (sort)
import List.Nonempty exposing (Nonempty, head, toList)
import List.Statistics as Stats


{-| Convert a List function to a no
-}
mapListOrHead : (List a -> Maybe a) -> (Nonempty a -> a)
mapListOrHead fn list =
    list
        |> toList
        |> fn
        |> Maybe.withDefault (head list)


mapList : (List a -> Maybe b) -> (Nonempty a -> Maybe b)
mapList fn list =
    list |> toList |> fn


{-| Wrapper for List.maximum
-}
maximum : Nonempty number -> number
maximum =
    mapListOrHead List.maximum


{-| Wrapper for List.minimum
-}
minimum : Nonempty number -> number
minimum =
    mapListOrHead List.minimum


{-| Wrapper for List.sum
-}
sum : Nonempty number -> number
sum list =
    list
        |> toList
        |> List.sum


{-| Wrapper for List.product
-}
product : Nonempty number -> number
product list =
    list
        |> toList
        |> List.product


{-| Calculate the mean of a list of Float
-}
avg : Nonempty Float -> Float
avg list =
    list
        |> List.Nonempty.foldl avgFolder ( 0, 0 )
        |> (\( c, t ) -> t / c)


{-| Calculate the mean of a list of Int
-}
avgInt : Nonempty Int -> Int
avgInt list =
    list
        |> List.Nonempty.foldl avgFolder ( 0, 0 )
        |> (\( c, t ) -> t // c)


avgFolder : number -> ( number, number ) -> ( number, number )
avgFolder n ( count, total ) =
    ( count + 1, total + n )


{-| Alias for avg
-}
mean : Nonempty Float -> Float
mean =
    avg


{-| Alias for avgInt
-}
meanInt : Nonempty Int -> Int
meanInt =
    avgInt


{-| Get the median of a sorted list of Float

If the length of the list is even, the retun value is the average of the two
values at the middle of the list.
Returns `Nothing` if the list is empty

-}
median : Nonempty Float -> Float
median =
    mapListOrHead Stats.median


{-| Get the median of a sorted list of Int

If the length of the list is even, the retun value is the average of the two
values at the middle of the list.
Returns `Nothing` if the list is empty

-}
medianInt : Nonempty Int -> Int
medianInt =
    mapListOrHead Stats.medianInt


{-| Get minimum and maximum from list

Returns `Nothing` if list is empty

-}
minmax : Nonempty number -> ( number, number )
minmax list =
    ( minimum list, minimum list )


{-| Get a `Dict` containing the numbers from the list as keys
and the number of occurrences for each number as value
-}
occurrences : List number -> Dict number Int
occurrences list =
    list |> List.foldl occurencesFolder Dict.empty


occurencesFolder : number -> Dict number Int -> Dict number Int
occurencesFolder n dict =
    dict |> Dict.update n (\c -> c |> Maybe.withDefault 0 |> (+) 1 |> Just)


{-| Get the element at a position in percent from a list

If the percentage doesn't exactly match an element the value is interpolated
from the two closest elements

-}
percentile : Float -> List Float -> Maybe Float
percentile p sorted =
    let
        l =
            List.length sorted

        pos =
            (toFloat l - 1) * clamp 0 1 p

        weight =
            pos - toFloat (floor pos)

        rest =
            sorted |> List.drop (floor pos)
    in
    case rest of
        a :: b :: _ ->
            (a * (1 - weight)) + (b * weight) |> Just

        a :: [] ->
            Just a

        [] ->
            -- List was empty
            Nothing


{-| Get the element at a position in percent from a list

If the percentage doesn't exactly match an element the value is interpolated
from the two closest elements

-}
percentileInt : Float -> Nonempty Int -> Int
percentileInt p =
    mapListOrHead (Stats.percentileInt p)


{-| Get elements at multiple positions in percent from a list

If the percentage doesn't exactly match an element the value is interpolated
from the two closest elements

-}
percentiles : List Float -> Nonempty Float -> List Float
percentiles ps sorted =
    sorted
        |> mapList (Stats.percentiles ps)
        |> Maybe.withDefault (ps |> List.map (\_ -> head sorted))


{-| Get elements at multiple positions in percent from a list

If the percentage doesn't exactly match an element the value is interpolated
from the two closest elements

-}
percentilesInt : List Float -> Nonempty Int -> List Int
percentilesInt ps sorted =
    sorted
        |> mapList (Stats.percentilesInt ps)
        |> Maybe.withDefault (ps |> List.map (\_ -> head sorted))


{-| Get The variance of a population of Float
-}
variance : Nonempty Float -> Float
variance population =
    mean population
        |> (\m ->
                let
                    ( count, total ) =
                        population
                            |> List.Nonempty.foldl
                                (\x ( n, sum_ ) ->
                                    ( n + 1, sum_ + ((x - m) ^ 2) )
                                )
                                ( 0, 0 )
                in
                total / count
           )


{-| Get The variance of a population of Int
This function uses mostly Int calculations wich can cause rounding errors.
See function `variance` (which uses Float) for more precise results.
-}
varianceInt : Nonempty Int -> Int
varianceInt population =
    meanInt population
        |> (\m ->
                let
                    ( count, total ) =
                        population
                            |> List.Nonempty.foldl
                                (\x ( n, sum_ ) ->
                                    ( n + 1, sum_ + ((x - m) ^ 2) )
                                )
                                ( 0, 0 )
                in
                (toFloat total / toFloat count) |> round
           )


{-| Get the standard deviation of a population of Float
-}
stdDeviation : Nonempty Float -> Float
stdDeviation population =
    population |> variance |> sqrt


{-| Get the standard deviation of a population of Int
-}
stdDeviationInt : Nonempty Int -> Int
stdDeviationInt population =
    population |> varianceInt |> (toFloat >> sqrt >> round)
