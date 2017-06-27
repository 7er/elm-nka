module Example exposing (..)

import Expect exposing (Expectation)


-- import Fuzz exposing (Fuzzer, list, int, string)

import Test exposing (..)
import Sykkeltiltak


closeTo : Float -> Int -> Float -> Expectation
closeTo expected precision actual =
    let
        epsilon =
            toFloat (10 ^ (negate precision)) / 2

        difference =
            abs (expected - actual)
    in
        if difference < epsilon then
            Expect.pass
        else
            (toString actual) ++ " is not near enough to " ++ (toString expected) ++ " using " ++ (toString precision) ++ " digits of precision" |> Expect.fail


suite : Test
suite =
    describe "Tiltak Models"
        [ describe "SykkelparkeringUteTiltak"
            [ test "calculates the nytte" <|
                \_ ->
                    let
                        model =
                            { tripsPerYear = 965
                            , yearlyMaintenance = 10000
                            , installationCost = 1.044111345e5
                            }
                    in
                        Sykkeltiltak.nytte model |> closeTo 543262.891 3
            ]
        , test "has no effect on a palindrome" <|
            \_ ->
                let
                    palindrome =
                        "hannah"
                in
                    Expect.equal palindrome (String.reverse palindrome)
        ]
