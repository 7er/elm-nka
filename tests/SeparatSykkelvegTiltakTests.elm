module SeparatSykkelvegTiltakTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (Test, describe, test)
import SeparatSykkelvegTiltak exposing (SeparatSykkelvegTiltakModel)


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


checkMaybe : (a -> Expectation) -> Maybe a -> Expectation
checkMaybe expectation maybeValue =
    case maybeValue of
        Just value ->
            expectation value

        Nothing ->
            Expect.fail "Got nothing"


suite : Test
suite =
    describe "SeparatSykkelvegTiltak"
        [ separatSykkelvegTest "calculates the kost" <|
            \model ->
                let
                    invKost =
                        35467795.5896
                in
                    SeparatSykkelvegTiltak.kostByInvestmentCost invKost |> closeTo 101529863.6 2
        , separatSykkelvegTest "calculates the nettoNytte" <|
            \model ->
                SeparatSykkelvegTiltak.nettoNytte { model | tripsPerYear = Just 775746 } |> checkMaybe (closeTo -675.2 1)
        ]


separatSykkelvegTest : String -> (SeparatSykkelvegTiltakModel {} -> Expectation) -> Test
separatSykkelvegTest description testCase =
    let
        model =
            { lengthKm = Just 2
            , tripsPerYear = Just 775747
            , minutesSaved = Just 0.0
            , investmentCost = Just 1.8297283503033772e8
            }
    in
        test description <|
            \_ ->
                testCase model
