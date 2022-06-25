module List.Statistics exposing
    ( avg, avgInt, mean, meanInt
    , median, medianInt, percentile, percentileInt
    , percentiles, percentilesInt
    , minimum, maximum, minmax
    , occurrences
    , variance, varianceInt
    , stdDeviation, stdDeviationInt
    , atLeast, atMost, sum, product
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

@docs atLeast, atMost, sum, product

-}

import Dict exposing (Dict)


{-| Alias for `max`, to make piped usage more intuitive

    number |> atLeast 5

-}
atLeast : number -> number -> number
atLeast =
    max


{-| Alias for `min`, to make piped usage more intuitive

    number |> atMost 5

-}
atMost : number -> number -> number
atMost =
    min


{-| Alias for List.maximum
-}
maximum : List number -> Maybe number
maximum =
    List.maximum


{-| Alias for List.minimum
-}
minimum : List number -> Maybe number
minimum =
    List.minimum


{-| Alias for List.sum
-}
sum : List number -> number
sum =
    List.sum


{-| Alias for List.product
-}
product : List number -> number
product =
    List.product


{-| Calculate the mean of a list of Float
-}
avg : List Float -> Maybe Float
avg list =
    case list of
        [] ->
            Nothing

        _ ->
            list
                |> List.foldl avgFolder ( 0, 0 )
                |> (\( c, t ) -> t / c)
                |> Just


{-| Calculate the mean of a list of Int
-}
avgInt : List Int -> Maybe Int
avgInt list =
    case list of
        [] ->
            Nothing

        _ ->
            list
                |> List.foldl avgFolder ( 0, 0 )
                |> (\( c, t ) -> t // c)
                |> Just


avgFolder : number -> ( number, number ) -> ( number, number )
avgFolder n ( count, total ) =
    ( count + 1, total + n )


{-| Alias for avg
-}
mean : List Float -> Maybe Float
mean =
    avg


{-| Alias for avgInt
-}
meanInt : List Int -> Maybe Int
meanInt =
    avgInt


{-| Get the median of a sorted list of Float

If the length of the list is even, the retun value is the average of the two
values at the middle of the list.
Returns `Nothing` if the list is empty

-}
median : List Float -> Maybe Float
median sorted =
    let
        l =
            List.length sorted

        rest =
            sorted |> List.drop ((l - 1) // 2)
    in
    if modBy 2 l == 1 then
        rest |> List.head

    else
        case rest of
            a :: b :: _ ->
                (a + b) / 2 |> Just

            _ ->
                -- List was empty
                Nothing


{-| Get the median of a sorted list of Int

If the length of the list is even, the retun value is the average of the two
values at the middle of the list.
Returns `Nothing` if the list is empty

-}
medianInt : List Int -> Maybe Int
medianInt sorted =
    let
        l =
            List.length sorted

        rest =
            sorted |> List.drop ((l - 1) // 2)
    in
    if modBy 2 l == 1 then
        rest |> List.head

    else
        case rest of
            a :: b :: _ ->
                (a + b) // 2 |> Just

            _ ->
                -- List was empty
                Nothing


{-| Get minimum and maximum from list

Returns `Nothing` if list is empty

-}
minmax : List number -> Maybe ( number, number )
minmax list =
    let
        min =
            list |> List.minimum

        max =
            list |> List.maximum
    in
    case ( min, max ) of
        ( Just a, Just b ) ->
            Just ( a, b )

        _ ->
            Nothing



{-
   Previous implementation was about 70% slower!:
   ```
    minmax list =
        case list of
            [] ->
                Nothing

            x :: xs ->
                xs |> List.foldl minmaxFolder ( x, x ) |> Just


    minmaxFolder : number -> ( number, number ) -> ( number, number )
    minmaxFolder n ( low, high ) =
        ( min low n, max high n )
   ```
-}


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
percentileInt : Float -> List Int -> Maybe Int
percentileInt p sorted =
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
            ((toFloat a * (1 - weight)) + (toFloat b * weight))
                -- use truncate for same behaviour as int division (`//`)
                |> truncate
                |> Just

        a :: [] ->
            Just a

        [] ->
            -- List was empty
            Nothing


{-| Get elements at multiple positions in percent from a list

If the percentage doesn't exactly match an element the value is interpolated
from the two closest elements

-}
percentiles : List Float -> List Float -> Maybe (List Float)
percentiles ps sorted =
    let
        pss =
            ps |> List.sort |> List.map (clamp 0 1)

        l =
            sorted |> List.length
    in
    pss
        |> List.foldl (percentilesHelper l) (Just ( 0, sorted, [] ))
        |> Maybe.map (\( _, _, c ) -> c)
        |> Maybe.map List.reverse


type alias PercentilesAcc =
    ( Int, List Float, List Float )


percentilesHelper : Int -> Float -> Maybe PercentilesAcc -> Maybe PercentilesAcc
percentilesHelper length p maybeAcc =
    case maybeAcc of
        Nothing ->
            Nothing

        Just ( dropped, sorted, acc ) ->
            let
                pos =
                    (toFloat length - 1) * p

                index =
                    floor pos

                weight =
                    pos - toFloat index

                rest =
                    sorted |> List.drop (index - dropped)
            in
            case rest of
                a :: b :: _ ->
                    Just ( index, rest, (a * (1 - weight)) + (b * weight) :: acc )

                a :: [] ->
                    Just ( index, rest, a :: acc )

                [] ->
                    -- List was empty
                    Nothing


{-| Get elements at multiple positions in percent from a list

If the percentage doesn't exactly match an element the value is interpolated
from the two closest elements

-}
percentilesInt : List Float -> List Int -> Maybe (List Int)
percentilesInt ps sorted =
    let
        pss =
            ps |> List.sort |> List.map (clamp 0 1)

        l =
            sorted |> List.length
    in
    pss
        |> List.foldl (percentilesIntHelper l) (Just ( 0, sorted, [] ))
        |> Maybe.map (\( _, _, c ) -> c)
        |> Maybe.map List.reverse


type alias PercentilesIntAcc =
    ( Int, List Int, List Int )


percentilesIntHelper : Int -> Float -> Maybe PercentilesIntAcc -> Maybe PercentilesIntAcc
percentilesIntHelper length p maybeAcc =
    case maybeAcc of
        Nothing ->
            Nothing

        Just ( dropped, sorted, acc ) ->
            let
                pos =
                    (toFloat length - 1) * p

                index =
                    floor pos

                weight =
                    pos - toFloat index

                rest =
                    sorted |> List.drop (index - dropped)
            in
            case rest of
                a :: b :: _ ->
                    ((toFloat a * (1 - weight)) + (toFloat b * weight))
                        -- use truncate for same behaviour as int division (`//`)
                        |> truncate
                        |> (\value -> ( index, rest, value :: acc ))
                        |> Just

                a :: [] ->
                    Just ( index, rest, a :: acc )

                [] ->
                    -- List was empty
                    Nothing


{-| Get The variance of a population of Float
-}
variance : List Float -> Maybe Float
variance population =
    mean population
        |> Maybe.map
            (\m ->
                let
                    ( count, total ) =
                        population
                            |> List.foldl
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
varianceInt : List Int -> Maybe Int
varianceInt population =
    meanInt population
        |> Maybe.map
            (\m ->
                let
                    ( count, total ) =
                        population
                            |> List.foldl
                                (\x ( n, sum_ ) ->
                                    ( n + 1, sum_ + ((x - m) ^ 2) )
                                )
                                ( 0, 0 )
                in
                (toFloat total / toFloat count) |> round
            )


{-| Get the standard deviation of a population of Float
-}
stdDeviation : List Float -> Maybe Float
stdDeviation population =
    population |> variance |> Maybe.map sqrt


{-| Get the standard deviation of a population of Int
-}
stdDeviationInt : List Int -> Maybe Int
stdDeviationInt population =
    population |> varianceInt |> Maybe.map (toFloat >> sqrt >> round)
