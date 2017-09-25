module NewtonsMethodTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (Test, describe, test)
import TestSupport exposing (checkMaybe, closeTo)
import Tiltak exposing (breakEvenPoint, roundToStepSize)


suite : Test
suite =
    Test.concat
        ([ describe "roundToStep"
            [ test "roundUp" <|
                \() ->
                    12.5 |> roundToStepSize 5 |> Expect.equal 15
            , test "roundDown" <|
                \() ->
                    12.4 |> roundToStepSize 5 |> Expect.equal 10
            ]
         , describe
            "breakEvenPoint"
            [ test "identity has breakEvenPoint zero" <|
                \() ->
                    breakEvenPoint identity
                        |> Expect.equal (Just 0)
            , test "x+2" <|
                \() ->
                    breakEvenPoint (\x -> x + 2)
                        |> Expect.equal (Just -2)
            , test "constant 42" <|
                \() ->
                    breakEvenPoint (\x -> 42)
                        |> Expect.equal Nothing
            , test "2x" <|
                \() ->
                    breakEvenPoint (\x -> 2 * x)
                        |> Expect.equal (Just 0)
            , test "2x-2" <|
                \() ->
                    breakEvenPoint (\x -> 2 * x - 2)
                        |> Expect.equal (Just 1)
            , test "-2x-2" <|
                \() ->
                    breakEvenPoint (\x -> -2 * x - 2)
                        |> Expect.equal (Just -1)
            , test "-2x+4" <|
                \() ->
                    breakEvenPoint (\x -> -2 * x + 4)
                        |> checkMaybe (Expect.equal 2)
            , test "3.14x+4" <|
                \() ->
                    breakEvenPoint (\x -> 3.14 * x + 4)
                        |> checkMaybe (closeTo -1.274 3)
            ]
         ]
        )
