module LeskurMedSitteplassTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (Test, describe, test)
import TestSupport exposing (..)
import Tiltak.LeskurMedSitteplass exposing (tiltak)
import Tiltak
import TiltakAndGroupData


suite : Test
suite =
    let
        initialState =
            TiltakAndGroupData.initialTiltakStates

        state =
            { initialState
                | leskurMedSitteplass =
                    { installationCost = Just 100
                    , yearlyMaintenance = Just 200
                    , passengersPerYear = Just 30
                    }
            }

        checkWithState description accessor expectation =
            test description <|
                \() -> Tiltak.sendTo tiltak accessor state |> checkMaybe expectation
    in
        describe "LeskurMedSitteplass"
            [ describe "nytte calculcations"
                [ checkWithState "passasjerNytte" .passasjerNytte (closeTo 4257.35 2)
                , checkWithState "yearlyPassasjerNytte" .yearlyPassasjerNytte (closeTo 174.35 2)
                ]
            ]
