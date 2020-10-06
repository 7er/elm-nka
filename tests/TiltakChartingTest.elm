module TiltakChartingTest exposing (suite)

import Expect exposing (Expectation)
import Test exposing (Test, describe, test, only, skip)
import Tiltak exposing (TiltakAccessor, sendTo)
import FormattedValue exposing (formattedValue)
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
                        { installationCost = Just 100 |> formattedValue
                        , yearlyMaintenance = Just 200 |> formattedValue
                        , bompengeAndel = 0.2
                        , passengersPerYear = Nothing |> formattedValue
                        , beleggForbiPassasjererPerBuss = Just 20 |> formattedValue
                        , yearlyTidsbesparelseMinutter = Just 30 |> formattedValue
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
                            [ ( 0, 17920.795510263626 )
                            , ( 50, 19089.54839671369 )
                            , ( 100, 20258.301283163753 )
                            , ( 150, 21427.054169613817 )
                            , ( 200, 22595.80705606388 )
                            , ( 250, 23764.559942513944 )
                            , ( 300, 24933.312828964008 )
                            , ( 350, 26102.065715414064 )
                            , ( 400, 27270.818601864128 )
                            ]
            , describe "maybeFieldToGraph"
                [ test "passengersPerYear" <|
                    \() ->
                        TiltakCharting.maybeFieldToGraph tiltak state
                            |> Expect.equal (Just passengersPerYear)
                , test "all fields are valid chooses the last chosen field" <|
                    \() ->
                        let
                            opphoeyetHoldeplassFelt =
                                state.opphoeyetHoldeplass

                            modifiedState =
                                { state
                                    | opphoeyetHoldeplass =
                                        { opphoeyetHoldeplassFelt
                                            | passengersPerYear = Just 10 |> formattedValue
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
                                            | yearlyMaintenance =
                                                Nothing
                                                    |> formattedValue
                                        }
                                }
                        in
                            TiltakCharting.maybeFieldToGraph tiltak modifiedState
                                |> Expect.equal Nothing
                ]
            ]
        )
