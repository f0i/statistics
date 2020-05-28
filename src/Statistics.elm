module Statistics exposing
    ( avg, mean, median, percentile
    , minimum, maximum, minmax
    , occurrences, stdDeviation
    , atLeast, atMost
    )

{-| Statistics functions for elm


# Average

@docs avg, mean, median, percentile


# Minimum / maximum

@docs minimum, maximum, minmax


# Distribution

@docs occurrences, stdDeviation


#

-}

import Dict exposing (Dict)


atLeast =
    max


atMost =
    min


maximum =
    List.maximum


minimum =
    List.minimum


avg : List Float -> Maybe Float
avg list =
    case list of
        [] ->
            Nothing

        _ ->
            list
                |> List.foldl avgFolder ( 0, 0.0 )
                |> (\( c, t ) -> t / c)
                |> Just


avgFolder n ( count, total ) =
    ( count + 1, total + n )


mean =
    avg


{-| Get the median of a sorted list of floats
if the length of the list is even, the retun value is the average of the two
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
            a :: b :: xs ->
                (a + b) / 2 |> Just

            _ ->
                -- List was empty
                Nothing


{-| Get minimum and maximum from list
-}
minmax : List number -> Maybe ( number, number )
minmax list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            xs |> List.foldl minmaxFolder ( x, x ) |> Just


minmaxFolder n ( low, high ) =
    ( min low n, max high n )


occurrences : List number -> Dict number Int
occurrences list =
    list |> List.foldl countFolder Dict.empty


countFolder n dict =
    dict |> Dict.update n (\c -> c |> Maybe.withDefault 0 |> (+) 1 |> Just)


percentile : Float -> List Float -> Maybe Float
percentile p sorted =
    let
        l =
            List.length sorted

        pos =
            (toFloat l - 1) * p

        weight =
            pos - toFloat (floor pos)

        rest =
            sorted |> List.drop (floor pos)
    in
    case rest of
        a :: b :: xs ->
            (a * (1 - weight)) + (b * weight) |> Just

        a :: [] ->
            Just a

        [] ->
            -- List was empty
            Nothing


stdDeviation : List Float -> Maybe Float
stdDeviation list =
    case mean list of
        Nothing ->
            Nothing

        Just m ->
            let
                ( count, total ) =
                    list |> List.foldl (\x ( n, sum ) -> ( n + 1, sum + ((x - m) ^ 2) )) ( 0, 0 )
            in
            Just <| sqrt (total / count)
