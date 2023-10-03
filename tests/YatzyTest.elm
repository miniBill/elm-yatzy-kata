module YatzyTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Yatzy


suite : Test
suite =
    describe "Yatzi"
        [ chance
        , counting
        , repetitions
        , straights
        ]


chance : Test
chance =
    group "Chance"
        [ Yatzy.chance 2 3 4 5 1
            |> Expect.equal 15
        , Yatzy.chance 3 3 4 5 1
            |> Expect.equal 16
        ]


group : String -> List Expect.Expectation -> Test
group label expectations =
    expectations
        |> List.indexedMap
            (\i expectation ->
                test ("#" ++ String.fromInt i) <| \_ -> expectation
            )
        |> describe label


repetitions : Test
repetitions =
    describe "Repetition"
        [ pair
        , twoPairs
        , triple
        , fourple
        , fullHouse
        , yatzy
        ]


pair : Test
pair =
    group "One pair"
        [ Yatzy.pair 3 4 3 5 6
            |> Expect.equal 6
        , Yatzy.pair 5 3 3 3 5
            |> Expect.equal 10
        , Yatzy.pair 5 3 6 6 5
            |> Expect.equal 12
        ]


twoPairs : Test
twoPairs =
    group "Two pairs"
        [ Yatzy.twoPairs 3 3 5 4 5
            |> Expect.equal 16
        , Yatzy.twoPairs 3 3 5 5 5
            |> Expect.equal 16
        ]


triple : Test
triple =
    group "Three of a kind"
        [ Yatzy.threeOfAKind 3 3 3 4 5
            |> Expect.equal 9
        , Yatzy.threeOfAKind 5 3 5 4 5
            |> Expect.equal 15
        , Yatzy.threeOfAKind 3 3 3 3 5
            |> Expect.equal 9
        ]


fourple : Test
fourple =
    group "Four of a kind"
        [ Yatzy.fourOfAKind 3 3 3 3 5
            |> Expect.equal 12
        , Yatzy.fourOfAKind 5 5 5 4 5
            |> Expect.equal 20
        , Yatzy.fourOfAKind 3 3 3 3 3
            |> Expect.equal 12
        ]


fullHouse : Test
fullHouse =
    group "Full house"
        [ Yatzy.fullHouse 6 2 2 2 6
            |> Expect.equal 18
        , Yatzy.fullHouse 2 3 4 5 6
            |> Expect.equal 0
        ]


yatzy : Test
yatzy =
    group "Yatzy scores 50"
        [ Yatzy.yatzy [ 4, 4, 4, 4, 4 ]
            |> Expect.equal 50
        , Yatzy.yatzy [ 6, 6, 6, 6, 6 ]
            |> Expect.equal 50
        , Yatzy.yatzy [ 6, 6, 6, 6, 3 ]
            |> Expect.equal 0
        ]


counting : Test
counting =
    describe "Counting"
        [ ones
        , twos
        , threes
        , fours
        , fives
        , sixes
        ]


ones : Test
ones =
    group "Ones"
        [ Yatzy.ones 1 2 3 4 5
            |> Expect.equal 1
        , Yatzy.ones 1 2 1 4 5
            |> Expect.equal 2
        , Yatzy.ones 6 2 2 4 5
            |> Expect.equal 0
        , Yatzy.ones 1 2 1 1 1
            |> Expect.equal 4
        ]


twos : Test
twos =
    group "Twos"
        [ Yatzy.twos 1 2 3 2 6
            |> Expect.equal 4
        , Yatzy.twos 2 2 2 2 2
            |> Expect.equal 10
        ]


threes : Test
threes =
    group "Threes"
        [ Yatzy.threes 1 2 3 2 3
            |> Expect.equal 6
        , Yatzy.threes 2 3 3 3 3
            |> Expect.equal 12
        ]


fours : Test
fours =
    group "Fours"
        [ Yatzy.init 4 4 4 5 5
            |> Yatzy.fours
            |> Expect.equal 12
        , Yatzy.init 4 4 5 5 5
            |> Yatzy.fours
            |> Expect.equal 8
        , Yatzy.init 4 5 5 5 5
            |> Yatzy.fours
            |> Expect.equal 4
        ]


fives : Test
fives =
    group "Fives"
        [ Yatzy.init 4 4 4 5 5
            |> Yatzy.fives
            |> Expect.equal 10
        , Yatzy.init 4 4 5 5 5
            |> Yatzy.fives
            |> Expect.equal 15
        , Yatzy.init 4 5 5 5 5
            |> Yatzy.fives
            |> Expect.equal 20
        ]


sixes : Test
sixes =
    group "Sixes"
        [ Yatzy.init 4 4 4 5 5
            |> Yatzy.sixes
            |> Expect.equal 0
        , Yatzy.init 4 4 6 5 5
            |> Yatzy.sixes
            |> Expect.equal 6
        , Yatzy.init 6 5 6 6 5
            |> Yatzy.sixes
            |> Expect.equal 18
        ]


straights : Test
straights =
    describe "Straights"
        [ largeStraight
        , smallStraight
        ]


smallStraight : Test
smallStraight =
    group "Small straight"
        [ Yatzy.smallStraight 1 2 3 4 5
            |> Expect.equal 15
        , Yatzy.smallStraight 2 3 4 5 1
            |> Expect.equal 15
        , Yatzy.smallStraight 1 2 2 4 5
            |> Expect.equal 0
        ]


largeStraight : Test
largeStraight =
    group "Large straight"
        [ Yatzy.largeStraight 6 2 3 4 5
            |> Expect.equal 20
        , Yatzy.largeStraight 2 3 4 5 6
            |> Expect.equal 20
        , Yatzy.largeStraight 1 2 2 4 5
            |> Expect.equal 0
        ]
