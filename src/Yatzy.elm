module Yatzy exposing (Yatzi, chance, fives, fourOfAKind, fours, fullHouse, init, largeStraight, ones, scorePair, sixes, smallStraight, threeOfAKind, threes, twoPair, twos, yatzy)

import Dict exposing (Dict)
import Dict.Extra
import List.Extra


type Yatzi
    = Yatzi (List Int)


init : Int -> Int -> Int -> Int -> Int -> Yatzi
init d1 d2 d3 d4 d5 =
    [ d1, d2, d3, d4, d5 ]
        |> Yatzi


chance : Int -> Int -> Int -> Int -> Int -> Int
chance d1 d2 d3 d4 d5 =
    d1 + d2 + d3 + d4 + d5


countScore : Int -> List Int -> Int
countScore v dice =
    List.Extra.count
        (\die -> die == v)
        dice
        * v


ones : Int -> Int -> Int -> Int -> Int -> Int
ones d1 d2 d3 d4 d5 =
    countScore 1 [ d1, d2, d3, d4, d5 ]


twos : Int -> Int -> Int -> Int -> Int -> Int
twos d1 d2 d3 d4 d5 =
    countScore 2 [ d1, d2, d3, d4, d5 ]


threes : Int -> Int -> Int -> Int -> Int -> Int
threes d1 d2 d3 d4 d5 =
    countScore 3 [ d1, d2, d3, d4, d5 ]


fours : Yatzi -> Int
fours (Yatzi dice) =
    countScore 4 dice


fives : Yatzi -> Int
fives (Yatzi dice) =
    countScore 5 dice


sixes : Yatzi -> Int
sixes (Yatzi dice) =
    countScore 6 dice


fourOfAKind : Int -> Int -> Int -> Int -> Int -> Int
fourOfAKind d1 d2 d3 d4 d5 =
    let
        tallies : Dict Int Int
        tallies =
            tally d1 d2 d3 d4 d5
    in
    tallies
        |> Dict.Extra.find (\_ count -> count >= 4)
        |> Maybe.map (\( i, _ ) -> i * 4)
        |> Maybe.withDefault 0


tally : Int -> Int -> Int -> Int -> Int -> Dict Int Int
tally d1 d2 d3 d4 d5 =
    [ d1, d2, d3, d4, d5 ]
        |> Dict.Extra.frequencies


fullHouse : Int -> Int -> Int -> Int -> Int -> Int
fullHouse d1 d2 d3 d4 d5 =
    let
        tallies : Dict Int Int
        tallies =
            tally d1 d2 d3 d4 d5
    in
    case Dict.toList tallies of
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


scorePair : Int -> Int -> Int -> Int -> Int -> Int
scorePair d1 d2 d3 d4 d5 =
    let
        counts_5 : Dict Int Int
        counts_5 =
            tally d1 d2 d3 d4 d5

        go : Int -> Int
        go at =
            if at /= 6 then
                if unsafeGetDict counts_5 (6 - at) >= 2 then
                    (6 - at) * 2

                else
                    go (at + 1)

            else
                0
    in
    go 0


yatzy : List Int -> Int
yatzy dice =
    case dice of
        [] ->
            0

        first :: rest ->
            if List.all (\die -> die == first) rest then
                50

            else
                0


twoPair : Int -> Int -> Int -> Int -> Int -> Int
twoPair d1 d2 d3 d4 d5 =
    let
        tallies : Dict Int Int
        tallies =
            tally d1 d2 d3 d4 d5

        ( n, score ) =
            List.range 0 5
                |> List.foldl
                    (\i ( n_, score_ ) ->
                        if unsafeGetDict tallies (6 - i) >= 2 then
                            ( n_ + 1, score_ + 6 - i )

                        else
                            ( n_, score_ )
                    )
                    ( 0, 0 )
    in
    if n == 2 then
        score * 2

    else
        0


unsafeGetDict : Dict comparable number -> comparable -> number
unsafeGetDict dict index =
    Dict.get index dict |> Maybe.withDefault -1


threeOfAKind : Int -> Int -> Int -> Int -> Int -> Int
threeOfAKind d1 d2 d3 d4 d5 =
    tally d1 d2 d3 d4 d5
        |> Dict.Extra.find (\_ count -> count >= 3)
        |> Maybe.map (\( i, _ ) -> i * 3)
        |> Maybe.withDefault 0


smallStraight : Int -> Int -> Int -> Int -> Int -> Int
smallStraight d1 d2 d3 d4 d5 =
    if List.sort [ d1, d2, d3, d4, d5 ] == List.range 1 5 then
        15

    else
        0
