module OpphoeyetHoldeplassTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (Test, describe, test, only, skip)
import TestSupport exposing (..)
import Tiltak exposing (TiltakAccessor, sendTo)
import TiltakCharting
import Tiltak.OpphoeyetHoldeplass as OpphoeyetHoldeplass exposing (tiltak)
import TiltakAndGroupData
import FormattedValue exposing (formattedValue)


suite : Test
suite =
    let
        initialState =
            TiltakAndGroupData.initialTiltakStates

        state =
            { initialState
                | opphoeyetHoldeplass =
                    { installationCost = Just 100 |> formattedValue
                    , yearlyMaintenance = Just 200 |> formattedValue
                    , bompengeAndel = 0.2
                    , passengersPerYear = Just 10 |> formattedValue
                    , beleggForbiPassasjererPerBuss = Just 20 |> formattedValue
                    , aarligTidsbesparelseMinutter = Just 30 |> formattedValue
                    , preferredToGraph = ""
                    }
            }

        expectedRecord =
            { yearlyPassasjerNytte = 624.88
            , yearlyTrafikantNytte = 0
            , yearlyOperatoerNytte = 219.06
            , nytte = 20607.26
            , passasjerNytte = 15258.28
            , trafikantNytte = 0
            , operatoerNytte = 5348.98
            , investeringsKostInklRestverdi = -129.18
            , driftOgVedlihKost = -3958.55
            , kostUtenSkyggepris = -4087.73
            , skyggepris = -654.04
            , nettoNytte = 15865.49
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
        describe "OpphoeyetHoldeplass tiltakSuite"
            [ tiltakSuite checkWithState expectedRecord ]
