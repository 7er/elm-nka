module KollektivPrioriteringLyskryssTest exposing (suite)

import Test exposing (Test, describe, test)
import TestSupport exposing (..)
import Tiltak
import FormattedValue exposing (formattedValue)
import Tiltak.KollektivPrioriteringLyskryss as KollektivPrioriteringLyskryss
import TiltakAndGroupData


suite : Test
suite =
    describe "KollektivPrioriteringLyskryss" <|
        let
            initialState =
                TiltakAndGroupData.initialTiltakStates

            state =
                { initialState
                    | kollektivPrioriteringLyskryss =
                        { installationCost = Just 100 |> formattedValue
                        , yearlyMaintenance = Just 200 |> formattedValue
                        , passengersPerYear = Just 30 |> formattedValue
                        , bompengeAndel = 0
                        , antallBilerForsinketPerAvgang = Just 3 |> formattedValue
                        , antallPasserendeAvgangerPerYear = Just 1000 |> formattedValue
                        , forsinkelsePerBilSeconds = Just 2 |> formattedValue
                        , preferredToGraph = ""
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
            [ describe "nytte calculcations"
                [ checkWithState
                    "passasjerNytte"
                    .passasjerNytte
                    (closeTo 289.94 2)
                , checkWithState
                    "yearlyPassasjerNytte"
                    .yearlyPassasjerNytte
                    (closeTo 12.9 2)
                , checkWithState
                    "yearlyTrafikantNytte"
                    .yearlyTrafikantNytte
                    (closeTo -246 2)
                , checkWithState
                    "trafikantNytte"
                    .trafikantNytte
                    (closeTo -5529.1 2)
                , checkWithState
                    "yearlyOperatoerNytte"
                    .yearlyOperatoerNytte
                    (closeTo 2603.33 2)
                , checkWithState
                    "operatoerNytte"
                    .operatoerNytte
                    (closeTo 58512.56 2)
                , checkWithState "nytte" .nytte (closeTo 53273.4 2)
                ]
            , describe "kost calculations"
                [ checkWithState
                    "investeringsKostInklRestverdi"
                    .investeringsKostInklRestverdi
                    (closeTo -179.42 2)
                , checkWithState
                    "driftOgVedlihKost"
                    .driftOgVedlihKost
                    (closeTo -3958.55 2)
                , checkWithState
                    "kostUtenSkyggepris"
                    .kostUtenSkyggepris
                    (closeTo -4137.97 2)
                , checkWithState
                    "skyggepris"
                    .skyggepris
                    (closeTo -827.59 2)
                ]
            , describe "nettonytte calculations"
                [ checkWithState
                    "nettoNytte"
                    .nettoNytte
                    (closeTo 48307.84 2)
                ]
            ]
