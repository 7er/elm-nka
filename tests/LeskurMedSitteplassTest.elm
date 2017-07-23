module LeskurMedSitteplassTest exposing (..)

-- import Expect exposing (Expectation)

import Test exposing (Test, describe, test)
import TestSupport exposing (..)
import Tiltak.LeskurMedSitteplass as LeskurMedSitteplass exposing (tiltak)
import GeneralForutsetninger
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
                [ checkWithState
                    "passasjerNytte"
                    .passasjerNytte
                    (closeTo 4257.35 2)
                , checkWithState
                    "yearlyPassasjerNytte"
                    .yearlyPassasjerNytte
                    (closeTo 174.35 2)
                , checkWithState "nytte" .nytte (closeTo 4257.35 2)
                ]
            , describe "kost calculations"
                [ checkWithState
                    "kostUtenSkyggepris"
                    .kostUtenSkyggepris
                    (closeTo -4170.51 2)
                , checkWithState
                    "driftOgVedlihKost"
                    .driftOgVedlihKost
                    (closeTo -3958.55 2)
                , checkWithState
                    "investeringsKostInklRestverdi"
                    .investeringsKostInklRestverdi
                    (closeTo -211.95 2)
                , test "investeringsFaktor" <|
                    \_ ->
                        GeneralForutsetninger.investeringsFaktor 12 |> closeTo 2.1195279 7
                ]
            ]
