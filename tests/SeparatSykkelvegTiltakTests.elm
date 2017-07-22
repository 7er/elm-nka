module SeparatSykkelvegTiltakTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (Test, describe, test)
import TestSupport exposing (..)
import Tiltak.SeparatSykkelveg as SeparatSykkelveg
import TiltakStates exposing (SeparatSykkelvegState)


suite : Test
suite =
    describe "SeparatSykkelveg"
        [ separatSykkelvegTest "calculates the kost" <|
            \model ->
                let
                    invKost =
                        35467795.5896
                in
                    SeparatSykkelveg.kostByInvestmentCost invKost
                        |> closeTo 101529863.6 2

        {- , separatSykkelvegTest "calculates the nettoNytte" <|
           \model ->
               SeparatSykkelveg.nettoNytte { model | tripsPerYear = Just 775746 } |> checkMaybe (closeTo -675.2 1)
        -}
        ]


separatSykkelvegTest : String -> (SeparatSykkelvegState -> Expectation) -> Test
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
