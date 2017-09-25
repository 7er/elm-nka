module OpphoeyetHoldeplassTest exposing (suite, tiltakSuiteTest)

import Expect exposing (Expectation)
import Test exposing (Test, describe, test, only, skip)
import TestSupport exposing (..)
import Tiltak exposing (TiltakAccessor, sendTo)
import Tiltak.OpphoeyetHoldeplass as OpphoeyetHoldeplass exposing (tiltak)
import TiltakAndGroupData


tiltakSuiteTest : Test
tiltakSuiteTest =
    let
        initialState =
            TiltakAndGroupData.initialTiltakStates

        state =
            { initialState
                | opphoeyetHoldeplass =
                    { installationCost = Just 100
                    , yearlyMaintenance = Just 200
                    , bompengeAndel = 0.2
                    , passengersPerYear = Just 10
                    , beleggForbiPassasjererPerBuss = Just 20
                    , aarligTidsbesparelseMinutter = Just 30
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


suite : Test
suite =
    describe "OpphoeyetHoldeplass"
        [ describe "charting"
            (let
                initialState =
                    TiltakAndGroupData.initialTiltakStates

                state =
                    { initialState
                        | opphoeyetHoldeplass =
                            { installationCost = Just 100
                            , yearlyMaintenance = Just 200
                            , bompengeAndel = 0.2
                            , passengersPerYear = Nothing
                            , beleggForbiPassasjererPerBuss = Just 20
                            , aarligTidsbesparelseMinutter = Just 30
                            }
                    }

                maybeField =
                    sendTo tiltak .fields |> List.filter (\field -> field.name == "passengersPerYear") |> List.head

                passengersPerYear =
                    case maybeField of
                        Just value ->
                            value

                        Nothing ->
                            Debug.crash "TODO"
             in
                [ skip <|
                    test "graphFor" <|
                        \() ->
                            state
                                |> sendTo tiltak .graphData
                                |> Expect.equal [ ( 1, 2 ), ( 3, 4 ) ]
                , describe "findVariableToGraph"
                    [ test "passengersPerYear" <|
                        \() ->
                            Tiltak.findVariableToGraph tiltak state
                                |> Expect.equal (Just passengersPerYear)
                    , test "all fields are valid" <|
                        \() ->
                            let
                                opphoeyetHoldeplassFelt =
                                    state.opphoeyetHoldeplass

                                modifiedState =
                                    { state
                                        | opphoeyetHoldeplass =
                                            { opphoeyetHoldeplassFelt
                                                | passengersPerYear = Just 10
                                            }
                                    }
                            in
                                Tiltak.findVariableToGraph tiltak modifiedState
                                    |> Expect.equal Nothing
                    , test "two fields are invalid" <|
                        \() ->
                            let
                                opphoeyetHoldeplassFelt =
                                    state.opphoeyetHoldeplass

                                modifiedState =
                                    { state
                                        | opphoeyetHoldeplass =
                                            { opphoeyetHoldeplassFelt
                                                | yearlyMaintenance = Nothing
                                            }
                                    }
                            in
                                Tiltak.findVariableToGraph tiltak modifiedState
                                    |> Expect.equal Nothing
                    ]
                ]
            )
        ]
