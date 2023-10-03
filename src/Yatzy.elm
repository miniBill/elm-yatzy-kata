module Yatzy exposing (Yatzi, chance, fives, fourOfAKind, fours, fullHouse, largeStraight, new, ones, scorePair, sixes, smallStraight, threeOfAKind, threes, twoPair, twos, yatzy)

import Array exposing (Array)


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

        total_5 =
            total_4 + d5
    in
    total_5


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

        sum_5 =
            if d5 == 1 then
                sum_4 + 1

            else
                sum_4
    in
    sum_5


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

        sum_5 =
            if d5 == 2 then
                sum_4 + 2

            else
                sum_4
    in
    sum_5


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

        s_5 =
            if d5 == 3 then
                s_4 + 3

            else
                s_4
    in
    s_5


type Yatzi
    = Yatzi (Array Int)


new : Int -> Int -> Int -> Int -> Int -> Yatzi
new d1 d2 d3 d4 d_5 =
    Array.repeat 5 0
        |> Array.set 0 d1
        |> Array.set 1 d2
        |> Array.set 2 d3
        |> Array.set 3 d4
        |> Array.set 4 d_5
        |> Yatzi


fives : Yatzi -> Int
fives (Yatzi dice) =
    List.range 0 (Array.length dice)
        |> List.foldl
            (\i s ->
                if unsafeGet dice i == 5 then
                    s + 5

                else
                    s
            )
            0


fourOfAKind : Int -> Int -> Int -> Int -> Int -> Int
fourOfAKind d_1 d_2 d3 d4 d5 =
    let
        tallies =
            Array.repeat 6 0

        tallies_1 =
            tallies
                |> Array.set (d_1 - 1) (unsafeGet tallies (d_1 - 1) + 1)

        tallies_2 =
            tallies_1
                |> Array.set (d_2 - 1) (unsafeGet tallies_1 (d_2 - 1) + 1)

        tallies_3 =
            tallies_2
                |> Array.set (d3 - 1) (unsafeGet tallies_2 (d3 - 1) + 1)

        tallies_4 =
            tallies_3
                |> Array.set (d4 - 1) (unsafeGet tallies_3 (d4 - 1) + 1)

        tallies_5 =
            tallies_4
                |> Array.set (d5 - 1) (unsafeGet tallies_4 (d5 - 1) + 1)

        check i =
            if i < 6 then
                if unsafeGet tallies_5 i >= 4 then
                    (i + 1) * 4

                else
                    check (i + 1)

            else
                0
    in
    check 0


fours : Yatzi -> Int
fours (Yatzi dice) =
    List.range 0 (Array.length dice)
        |> List.foldl
            (\i s ->
                if unsafeGet dice i == 4 then
                    s + 4

                else
                    s
            )
            0


fullHouse : Int -> Int -> Int -> Int -> Int -> Int
fullHouse d1 d2 d3 d4 d5 =
    let
        tallies =
            Array.repeat 6 0

        tallies_1 =
            tallies
                |> Array.set (d1 - 1) (unsafeGet tallies (d1 - 1) + 1)

        tallies_2 =
            tallies_1
                |> Array.set (d2 - 1) (unsafeGet tallies_1 (d2 - 1) + 1)

        tallies_3 =
            tallies_2
                |> Array.set (d3 - 1) (unsafeGet tallies_2 (d3 - 1) + 1)

        tallies_4 =
            tallies_3
                |> Array.set (d4 - 1) (unsafeGet tallies_3 (d4 - 1) + 1)

        tallies_5 =
            tallies_4
                |> Array.set (d5 - 1) (unsafeGet tallies_4 (d5 - 1) + 1)

        ( t_2, t_2_at ) =
            List.range 0 5
                |> List.foldl
                    (\i acc ->
                        if unsafeGet tallies_5 i == 2 then
                            ( True, i + 1 )

                        else
                            acc
                    )
                    ( False, 0 )

        ( t_3, t_3_at ) =
            List.range 0 5
                |> List.foldl
                    (\i acc ->
                        if unsafeGet tallies_5 i == 3 then
                            ( True, i + 1 )

                        else
                            acc
                    )
                    ( False, 0 )
    in
    if t_2 && t_3 then
        t_2_at * 2 + t_3_at * 3

    else
        0


largeStraight : Int -> Int -> Int -> Int -> Int -> Int
largeStraight d1 d2 d3 d4 d5 =
    let
        tallies =
            Array.repeat 6 0

        tallies_1 =
            tallies
                |> Array.set (d1 - 1) (unsafeGet tallies (d1 - 1) + 1)

        tallies_2 =
            tallies_1
                |> Array.set (d2 - 1) (unsafeGet tallies_1 (d2 - 1) + 1)

        tallies_3 =
            tallies_2
                |> Array.set (d3 - 1) (unsafeGet tallies_2 (d3 - 1) + 1)

        tallies_4 =
            tallies_3
                |> Array.set (d4 - 1) (unsafeGet tallies_3 (d4 - 1) + 1)

        tallies_5 =
            tallies_4
                |> Array.set (d5 - 1) (unsafeGet tallies_4 (d5 - 1) + 1)
    in
    if
        (unsafeGet tallies_5 1 == 1)
            && (unsafeGet tallies_5 2 == 1)
            && (unsafeGet tallies_5 3 == 1)
            && (unsafeGet tallies_5 4 == 1)
            && (unsafeGet tallies_5 5 == 1)
    then
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
    List.range 0 (Array.length dice - 1)
        |> List.foldl
            (\at sum ->
                if unsafeGet dice at == 6 then
                    sum + 6

                else
                    sum
            )
            0


yatzy : List Int -> Int
yatzy dice =
    let
        counts =
            List.foldl
                (\die counts_ ->
                    Array.set (die - 1) (unsafeGet counts_ (die - 1) + 1) counts_
                )
                (Array.repeat 6 0)
                dice

        go i =
            if i /= 6 then
                if unsafeGet counts i == 5 then
                    50

                else
                    go (i + 1)

            else
                0
    in
    go 0


twoPair : Int -> Int -> Int -> Int -> Int -> Int
twoPair d1 d2 d3 d4 d5 =
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

        ( n, score ) =
            List.range 0 5
                |> List.foldl
                    (\i ( n_, score_ ) ->
                        if unsafeGet counts_5 (6 - i - 1) >= 2 then
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


threeOfAKind : Int -> Int -> Int -> Int -> Int -> Int
threeOfAKind d1 d2 d3 d4 d5 =
    let
        t =
            Array.repeat 6 0

        t_1 =
            t
                |> Array.set (d1 - 1) (unsafeGet t (d1 - 1) + 1)

        t_2 =
            t_1
                |> Array.set (d2 - 1) (unsafeGet t_1 (d2 - 1) + 1)

        t_3 =
            t_2
                |> Array.set (d3 - 1) (unsafeGet t_2 (d3 - 1) + 1)

        t_4 =
            t_3
                |> Array.set (d4 - 1) (unsafeGet t_3 (d4 - 1) + 1)

        t_5 =
            t_4
                |> Array.set (d5 - 1) (unsafeGet t_4 (d5 - 1) + 1)

        go i =
            if i < 6 then
                if unsafeGet t_5 i >= 3 then
                    (i + 1) * 3

                else
                    go (i + 1)

            else
                0
    in
    go 0


smallStraight : Int -> Int -> Int -> Int -> Int -> Int
smallStraight d1 d2 d3 d4 d5 =
    let
        tallies =
            Array.repeat 6 0

        tallies_1 =
            tallies
                |> Array.set (d1 - 1) (unsafeGet tallies (d1 - 1) + 1)

        tallies_2 =
            tallies_1
                |> Array.set (d2 - 1) (unsafeGet tallies_1 (d2 - 1) + 1)

        tallies_3 =
            tallies_2
                |> Array.set (d3 - 1) (unsafeGet tallies_2 (d3 - 1) + 1)

        tallies_4 =
            tallies_3
                |> Array.set (d4 - 1) (unsafeGet tallies_3 (d4 - 1) + 1)

        tallies_5 =
            tallies_4
                |> Array.set (d5 - 1) (unsafeGet tallies_4 (d5 - 1) + 1)
    in
    if unsafeGet tallies_5 0 == 1 && unsafeGet tallies_5 1 == 1 && unsafeGet tallies_5 2 == 1 && unsafeGet tallies_5 3 == 1 && unsafeGet tallies_5 4 == 1 then
        15

    else
        0


unsafeGet : Array number -> Int -> number
unsafeGet arr index =
    Array.get index arr
        |> Maybe.withDefault -1
