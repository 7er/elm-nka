module LaventrebussTest exposing (..)

import Test exposing (Test, describe, test, only, skip)
import TestSupport exposing (..)
import Tiltak exposing (TiltakAccessor, sendTo)
import Tiltak.Laventrebuss as Laventrebuss exposing (tiltak)
import TiltakAndGroupData
import FormattedValue exposing (formattedValue)


suite : Test
suite =
    let
        initialState =
            TiltakAndGroupData.initialTiltakStates

        state =
            { initialState
                | laventrebuss =
                    { installationCost = Just 1 |> formattedValue
                    , yearlyMaintenance = Just 2 |> formattedValue
                    , bompengeAndel = 0
                    , passengersPerYear = Just 3 |> formattedValue
                    , passasjererPerBuss = Just 4 |> formattedValue
                    , yearlyTidsbesparelseMinutter = Just 5 |> formattedValue
                    , passasjererTilpassedeHoldplasserPercent = Just 0.06 |> formattedValue
                    , preferredToGraph = ""
                    }
            }

        expectedRecord =
            { yearlyPassasjerNytte = 32.06
            , yearlyTrafikantNytte = 0
            , yearlyOperatoerNytte = 39.05
            , passasjerNytte = 720.5
            , trafikantNytte = 0
            , operatoerNytte = 877.69
            , nytte = 1598.18
            , investeringsKostInklRestverdi = -2.44
            , driftOgVedlihKost = -39.59
            , kostUtenSkyggepris = -42.03
            , skyggepris = -8.4052
            , nettoNytte = 1547.75
            }

        checkWithState : CheckWithStateFunction
        checkWithState description accessor expectation =
            test description <|
                \() ->
                    sendTo
                        tiltak
                        accessor
                        state
                        |> checkMaybe expectation
    in
        describe "Laventrebuss tiltakSuite"
            [ tiltakSuite checkWithState expectedRecord ]
