module TiltakChartingTest exposing (suite)

import Expect exposing (Expectation)
import Test exposing (Test, describe, test, only, skip)
import TestSupport exposing (..)
import Tiltak exposing (TiltakAccessor, sendTo)
import TiltakCharting
import Tiltak.OpphoeyetHoldeplass as OpphoeyetHoldeplass exposing (tiltak)
import TiltakAndGroupData


suite : Test
suite =
    describe "TiltakCharting"
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
                        , preferredToGraph = "passengersPerYear"
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
            [ test "graphFor" <|
                \() ->
                    state
                        |> TiltakCharting.graphData tiltak
                        |> Expect.equal
                            [ ( 0, 15633.141578094202 )
                            , ( 50, 16794.86876781479 )
                            , ( 100, 17956.595957535377 )
                            , ( 150, 19118.323147255964 )
                            , ( 200, 20280.05033697655 )
                            , ( 250, 21441.777526697144 )
                            , ( 300, 22603.50471641773 )
                            , ( 350, 23765.231906138317 )
                            , ( 400, 24926.95909585891 )
                            ]
            , describe "maybeFieldToGraph"
                [ test "passengersPerYear" <|
                    \() ->
                        TiltakCharting.maybeFieldToGraph tiltak state
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
                            TiltakCharting.maybeFieldToGraph tiltak modifiedState
                                |> Expect.equal (Just passengersPerYear)
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
                            TiltakCharting.maybeFieldToGraph tiltak modifiedState
                                |> Expect.equal Nothing
                ]
            ]
        )
