module Yatzy exposing (Yatzi, chance, fives, fourOfAKind, fours, fullHouse, largeStraight, new, ones, scorePair, sixes, smallStraight, threeOfAKind, threes, twoPair, twos, yatzy)

import Array exposing (Array)
import Dict exposing (Dict)
import Dict.Extra


chance : Int -> Int -> Int -> Int -> Int -> Int
chance d1 d2 d3 d4 d5 =
    let
        total =
            0

        total_1 =
            total + d1

        total_2 =
            total_1 + d2

        total_3 =
            total_2 + d3

        total_4 =
            total_3 + d4
    in
    total_4 + d5


ones : Int -> Int -> Int -> Int -> Int -> Int
ones d1 d2 d3 d4 d5 =
    let
        sum =
            0

        sum_1 =
            if d1 == 1 then
                sum + 1

            else
                sum

        sum_2 =
            if d2 == 1 then
                sum_1 + 1

            else
                sum_1

        sum_3 =
            if d3 == 1 then
                sum_2 + 1

            else
                sum_2

        sum_4 =
            if d4 == 1 then
                sum_3 + 1

            else
                sum_3
    in
    if d5 == 1 then
        sum_4 + 1

    else
        sum_4


twos : Int -> Int -> Int -> Int -> Int -> Int
twos d1 d2 d3 d4 d5 =
    let
        sum =
            0

        sum_1 =
            if d1 == 2 then
                sum + 2

            else
                sum

        sum_2 =
            if d2 == 2 then
                sum_1 + 2

            else
                sum_1

        sum_3 =
            if d3 == 2 then
                sum_2 + 2

            else
                sum_2

        sum_4 =
            if d4 == 2 then
                sum_3 + 2

            else
                sum_3
    in
    if d5 == 2 then
        sum_4 + 2

    else
        sum_4


threes : Int -> Int -> Int -> Int -> Int -> Int
threes d1 d2 d3 d4 d5 =
    let
        s =
            0

        s_1 =
            if d1 == 3 then
                s + 3

            else
                s

        s_2 =
            if d2 == 3 then
                s_1 + 3

            else
                s_1

        s_3 =
            if d3 == 3 then
                s_2 + 3

            else
                s_2

        s_4 =
            if d4 == 3 then
                s_3 + 3

            else
                s_3
    in
    if d5 == 3 then
        s_4 + 3

    else
        s_4


type Yatzi
    = Yatzi (List Int)


new : Int -> Int -> Int -> Int -> Int -> Yatzi
new d1 d2 d3 d4 d5 =
    [ d1, d2, d3, d4, d5 ]
        |> Yatzi


fives : Yatzi -> Int
fives (Yatzi dice) =
    dice
        |> List.foldl
            (\i s ->
                if i == 5 then
                    s + 5

                else
                    s
            )
            0


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


fours : Yatzi -> Int
fours (Yatzi dice) =
    dice
        |> List.foldl
            (\i s ->
                if i == 4 then
                    s + 4

                else
                    s
            )
            0


fullHouse : Int -> Int -> Int -> Int -> Int -> Int
fullHouse d1 d2 d3 d4 d5 =
    let
        tallies : Dict Int Int
        tallies =
            tally d1 d2 d3 d4 d5

        pair : Maybe ( Int, Int )
        pair =
            Dict.Extra.find
                (\_ count -> count == 2)
                tallies

        triple : Maybe ( Int, Int )
        triple =
            Dict.Extra.find
                (\_ count -> count == 3)
                tallies
    in
    case ( pair, triple ) of
        ( Just ( pairValue, _ ), Just ( tripleValue, _ ) ) ->
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
        counts =
            Array.repeat 6 0

        counts_1 =
            counts
                |> Array.set (d1 - 1) (unsafeGet counts (d1 - 1) + 1)

        counts_2 =
            counts_1
                |> Array.set (d2 - 1) (unsafeGet counts_1 (d2 - 1) + 1)

        counts_3 =
            counts_2
                |> Array.set (d3 - 1) (unsafeGet counts_2 (d3 - 1) + 1)

        counts_4 =
            counts_3
                |> Array.set (d4 - 1) (unsafeGet counts_3 (d4 - 1) + 1)

        counts_5 =
            counts_4
                |> Array.set (d5 - 1) (unsafeGet counts_4 (d5 - 1) + 1)

        go at =
            if at /= 6 then
                if unsafeGet counts_5 (6 - at - 1) >= 2 then
                    (6 - at) * 2

                else
                    go (at + 1)

            else
                0
    in
    go 0


sixes : Yatzi -> Int
sixes (Yatzi dice) =
    dice
        |> List.foldl
            (\at sum ->
                if at == 6 then
                    sum + 6

                else
                    sum
            )
            0


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


unsafeGet : Array number -> Int -> number
unsafeGet arr index =
    Array.get index arr
        |> Maybe.withDefault -1
