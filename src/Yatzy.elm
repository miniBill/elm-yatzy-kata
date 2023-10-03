module Yatzy exposing (chance, fives, fourOfAKind, fours, fullHouse, largeStraight, ones, pair, sixes, smallStraight, threeOfAKind, threes, twoPairs, twos, yatzy)

import Dict exposing (Dict)
import Dict.Extra
import List.Extra


chance : Int -> Int -> Int -> Int -> Int -> Int
chance d1 d2 d3 d4 d5 =
    d1 + d2 + d3 + d4 + d5


countScore : Int -> Int -> Int -> Int -> Int -> Int -> Int
countScore v d1 d2 d3 d4 d5 =
    List.Extra.count
        (\die -> die == v)
        [ d1, d2, d3, d4, d5 ]
        * v


ones : Int -> Int -> Int -> Int -> Int -> Int
ones d1 d2 d3 d4 d5 =
    countScore 1 d1 d2 d3 d4 d5


twos : Int -> Int -> Int -> Int -> Int -> Int
twos d1 d2 d3 d4 d5 =
    countScore 2 d1 d2 d3 d4 d5


threes : Int -> Int -> Int -> Int -> Int -> Int
threes d1 d2 d3 d4 d5 =
    countScore 3 d1 d2 d3 d4 d5


fours : Int -> Int -> Int -> Int -> Int -> Int
fours d1 d2 d3 d4 d5 =
    countScore 4 d1 d2 d3 d4 d5


fives : Int -> Int -> Int -> Int -> Int -> Int
fives d1 d2 d3 d4 d5 =
    countScore 5 d1 d2 d3 d4 d5


sixes : Int -> Int -> Int -> Int -> Int -> Int
sixes d1 d2 d3 d4 d5 =
    countScore 6 d1 d2 d3 d4 d5


tally : Int -> Int -> Int -> Int -> Int -> Dict Int Int
tally d1 d2 d3 d4 d5 =
    [ d1, d2, d3, d4, d5 ]
        |> Dict.Extra.frequencies


tallyList : Int -> Int -> Int -> Int -> Int -> List ( Int, Int )
tallyList d1 d2 d3 d4 d5 =
    [ d1, d2, d3, d4, d5 ]
        |> Dict.Extra.frequencies
        |> Dict.toList


fourOfAKind : Int -> Int -> Int -> Int -> Int -> Int
fourOfAKind d1 d2 d3 d4 d5 =
    tally d1 d2 d3 d4 d5
        |> Dict.Extra.find (\_ count -> count >= 4)
        |> Maybe.map (\( i, _ ) -> i * 4)
        |> Maybe.withDefault 0


fullHouse : Int -> Int -> Int -> Int -> Int -> Int
fullHouse d1 d2 d3 d4 d5 =
    case tallyList d1 d2 d3 d4 d5 of
        [ ( pairValue, 2 ), ( tripleValue, 3 ) ] ->
            pairValue * 2 + tripleValue * 3

        [ ( tripleValue, 3 ), ( pairValue, 2 ) ] ->
            pairValue * 2 + tripleValue * 3

        _ ->
            0


largeStraight : Int -> Int -> Int -> Int -> Int -> Int
largeStraight d1 d2 d3 d4 d5 =
    if List.sort [ d1, d2, d3, d4, d5 ] == List.range 2 6 then
        20

    else
        0


pair : Int -> Int -> Int -> Int -> Int -> Int
pair d1 d2 d3 d4 d5 =
    tallyList d1 d2 d3 d4 d5
        |> List.reverse
        |> List.Extra.find (\( _, count ) -> count >= 2)
        |> Maybe.map (\( die, _ ) -> die * 2)
        |> Maybe.withDefault 0


yatzy : Int -> Int -> Int -> Int -> Int -> Int
yatzy d1 d2 d3 d4 d5 =
    if d1 == d2 && d2 == d3 && d3 == d4 && d4 == d5 then
        50

    else
        0


twoPairs : Int -> Int -> Int -> Int -> Int -> Int
twoPairs d1 d2 d3 d4 d5 =
    case
        tallyList d1 d2 d3 d4 d5
            |> List.filter
                (\( _, count ) -> count >= 2)
    of
        [ ( first, _ ), ( second, _ ) ] ->
            (first + second) * 2

        _ ->
            0


threeOfAKind : Int -> Int -> Int -> Int -> Int -> Int
threeOfAKind d1 d2 d3 d4 d5 =
    tally d1 d2 d3 d4 d5
        |> Dict.Extra.find (\_ count -> count >= 3)
        |> Maybe.map (\( die, _ ) -> die * 3)
        |> Maybe.withDefault 0


smallStraight : Int -> Int -> Int -> Int -> Int -> Int
smallStraight d1 d2 d3 d4 d5 =
    if List.sort [ d1, d2, d3, d4, d5 ] == List.range 1 5 then
        15

    else
        0
