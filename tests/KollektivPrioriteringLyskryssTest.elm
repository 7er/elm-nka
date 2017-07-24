module KollektivPrioriteringLyskryssTest exposing (suite)

-- import Expect exposing (Expectation)

import Test exposing (Test, describe, test)
import TestSupport exposing (..)
import Tiltak.KollektivPrioriteringLyskryss as KollektivPrioriteringLyskryss


-- import GeneralForutsetninger

import Tiltak
import TiltakAndGroupData


suite : Test
suite =
    let
        initialState =
            TiltakAndGroupData.initialTiltakStates

        state =
            { initialState
                | kollektivPrioriteringLyskryss =
                    { installationCost = Nothing
                    , yearlyMaintenance = Nothing
                    , passengersPerYear = Just 30
                    , bompengeAndel = 0
                    , antallBilerForsinketPerAvgang = Just 3
                    , antallPasserendeAvgangerPerYear = Just 1000
                    , forsinkelsePerBilSeconds = Just 2
                    }
            }

        checkWithState description accessor expectation =
            test description <|
                \() ->
                    Tiltak.sendTo
                        KollektivPrioriteringLyskryss.tiltak
                        accessor
                        state
                        |> checkMaybe expectation
    in
        describe "KollektivPrioriteringLyskryss"
            [ describe "nytte calculcations"
                [ checkWithState
                    "passasjerNytte"
                    .passasjerNytte
                    (closeTo 250.43 2)
                , checkWithState
                    "yearlyPassasjerNytte"
                    .yearlyPassasjerNytte
                    (closeTo 10.26 2)
                , checkWithState
                    "yearlyTrafikantNytte"
                    .yearlyTrafikantNytte
                    (closeTo -153.84 2)
                , checkWithState
                    "trafikantNytte"
                    .trafikantNytte
                    (closeTo -3756.48 2)
                , checkWithState
                    "yearlyOperatoerNytte"
                    .yearlyOperatoerNytte
                    (closeTo 2433.99 2)
                , checkWithState
                    "operatoerNytte"
                    .operatoerNytte
                    (closeTo 59433.13 2)
                , checkWithState "nytte" .nytte (closeTo 55927.08 2)
                ]
            , describe "kost calculations"
                []
            ]
