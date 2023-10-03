module YatzyTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Yatzy


suite : Test
suite =
    describe "Yatzi"
        [ describe "Chance_scores_sum_of_all_dice"
            [ test "1" <|
                \_ ->
                    Yatzy.chance 2 3 4 5 1
                        |> Expect.equal 15
            , test "2" <|
                \_ ->
                    Yatzy.chance 3 3 4 5 1
                        |> Expect.equal 16
            ]
        , describe "Fact_1s"
            [ test "1" <|
                \_ ->
                    (Yatzy.ones 1 2 3 4 5 == 1)
                        |> Expect.equal True
            , test "2" <|
                \_ ->
                    Yatzy.ones 1 2 1 4 5
                        |> Expect.equal 2
            , test "3" <|
                \_ ->
                    Yatzy.ones 6 2 2 4 5
                        |> Expect.equal 0
            , test "4" <|
                \_ ->
                    Yatzy.ones 1 2 1 1 1
                        |> Expect.equal 4
            ]
        , describe "Fact_2s"
            [ test "1" <|
                \_ ->
                    Yatzy.twos 1 2 3 2 6
                        |> Expect.equal 4
            , test "2" <|
                \_ ->
                    Yatzy.twos 2 2 2 2 2
                        |> Expect.equal 10
            ]
        , describe "Fact_threes"
            [ test "1" <|
                \_ ->
                    Yatzy.threes 1 2 3 2 3
                        |> Expect.equal 6
            , test "2" <|
                \_ ->
                    Yatzy.threes 2 3 3 3 3
                        |> Expect.equal 12
            ]
        , describe "fives"
            [ test "1" <|
                \_ ->
                    Yatzy.init 4 4 4 5 5
                        |> Yatzy.fives
                        |> Expect.equal 10
            , test "2" <|
                \_ ->
                    Yatzy.init 4 4 5 5 5
                        |> Yatzy.fives
                        |> Expect.equal 15
            , test "3" <|
                \_ ->
                    Yatzy.init 4 5 5 5 5
                        |> Yatzy.fives
                        |> Expect.equal 20
            ]
        , describe "four_of_a_knd"
            [ test "1" <|
                \_ ->
                    Yatzy.fourOfAKind 3 3 3 3 5
                        |> Expect.equal 12
            , test "2" <|
                \_ ->
                    Yatzy.fourOfAKind 5 5 5 4 5
                        |> Expect.equal 20
            , test "3" <|
                \_ ->
                    Yatzy.fourOfAKind 3 3 3 3 3
                        |> Expect.equal 12
            ]
        , describe "fours_Fact"
            [ test "1" <|
                \_ ->
                    Yatzy.init 4 4 4 5 5
                        |> Yatzy.fours
                        |> Expect.equal 12
            , test "2" <|
                \_ ->
                    Yatzy.init 4 4 5 5 5
                        |> Yatzy.fours
                        |> Expect.equal 8
            , test "3" <|
                \_ ->
                    Yatzy.init 4 5 5 5 5
                        |> Yatzy.fours
                        |> Expect.equal 4
            ]
        , describe "fullHouse"
            [ test "1" <|
                \_ ->
                    Yatzy.fullHouse 6 2 2 2 6
                        |> Expect.equal 18
            , test "2" <|
                \_ ->
                    Yatzy.fullHouse 2 3 4 5 6
                        |> Expect.equal 0
            ]
        , describe "largeStraight"
            [ test "1" <|
                \_ ->
                    Yatzy.largeStraight 6 2 3 4 5
                        |> Expect.equal 20
            , test "2" <|
                \_ ->
                    Yatzy.largeStraight 2 3 4 5 6
                        |> Expect.equal 20
            , test "3" <|
                \_ ->
                    Yatzy.largeStraight 1 2 2 4 5
                        |> Expect.equal 0
            ]
        , describe "one_pair"
            [ test "1" <|
                \_ ->
                    Yatzy.scorePair 3 4 3 5 6
                        |> Expect.equal 6
            , test "2" <|
                \_ ->
                    Yatzy.scorePair 5 3 3 3 5
                        |> Expect.equal 10
            , test "3" <|
                \_ ->
                    Yatzy.scorePair 5 3 6 6 5
                        |> Expect.equal 12
            ]
        , describe "sixes_Fact"
            [ test "1" <|
                \_ ->
                    Yatzy.init 4 4 4 5 5
                        |> Yatzy.sixes
                        |> Expect.equal 0
            , test "2" <|
                \_ ->
                    Yatzy.init 4 4 6 5 5
                        |> Yatzy.sixes
                        |> Expect.equal 6
            , test "3" <|
                \_ ->
                    Yatzy.init 6 5 6 6 5
                        |> Yatzy.sixes
                        |> Expect.equal 18
            ]
        , describe "smallStraight"
            [ test "1" <|
                \_ ->
                    Yatzy.smallStraight 1 2 3 4 5
                        |> Expect.equal 15
            , test "2" <|
                \_ ->
                    Yatzy.smallStraight 2 3 4 5 1
                        |> Expect.equal 15
            , test "3" <|
                \_ ->
                    Yatzy.smallStraight 1 2 2 4 5
                        |> Expect.equal 0
            ]
        , describe "three_of_a_kind"
            [ test "1" <|
                \_ ->
                    Yatzy.threeOfAKind 3 3 3 4 5
                        |> Expect.equal 9
            , test "2" <|
                \_ ->
                    Yatzy.threeOfAKind 5 3 5 4 5
                        |> Expect.equal 15
            , test "3" <|
                \_ ->
                    Yatzy.threeOfAKind 3 3 3 3 5
                        |> Expect.equal 9
            ]
        , describe "two_Pair"
            [ test "1" <|
                \_ ->
                    Yatzy.twoPair 3 3 5 4 5
                        |> Expect.equal 16
            , test "2" <|
                \_ ->
                    Yatzy.twoPair 3 3 5 5 5
                        |> Expect.equal 16
            ]
        , describe "Yatzy_scores_50"
            [ let
                expected =
                    50

                actual =
                    Yatzy.yatzy [ 4, 4, 4, 4, 4 ]
              in
              test "1" <| \_ -> Expect.equal expected actual
            , test "2" <|
                \_ ->
                    Yatzy.yatzy [ 6, 6, 6, 6, 6 ]
                        |> Expect.equal 50
            , test "3" <|
                \_ ->
                    Yatzy.yatzy [ 6, 6, 6, 6, 3 ]
                        |> Expect.equal 0
            ]
        ]
