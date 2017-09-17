module NewtonsMethodTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (Test, describe, test)
import TestSupport exposing (checkMaybe, closeTo)


breakEvenPoint : (Float -> Float) -> Maybe Float
breakEvenPoint func =
    let
        y0 =
            func 0

        rise =
            func 1 - y0
    in
        case rise of
            0 ->
                Nothing

            _ ->
                negate y0 / rise |> Just


suite =
    describe "breakEvenPoint"
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
