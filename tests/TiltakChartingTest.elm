module TiltakChartingTest exposing (suite)

import Expect exposing (Expectation)
import FormattedValue exposing (formattedValue)
import Test exposing (Test, describe, only, skip, test)
import Tiltak exposing (TiltakAccessor, sendTo)
import Tiltak.OpphoeyetHoldeplass as OpphoeyetHoldeplass exposing (tiltak)
import TiltakAndGroupData
import TiltakCharting


precision =
    2


epsilon =
    toFloat (10 ^ negate precision) / 2


isCloseEnough expected actual =
    let
        difference =
            abs (expected - actual)
    in
    difference < epsilon


expectGraphDataToBeCloseTo expected actual =
    case expected of
        [] ->
            if List.isEmpty actual then
                Expect.pass

            else
                toString actual
                    ++ " has more items than"
                    ++ toString expected
                    |> Expect.fail

        ( expectedX, expectedY ) :: expectedTail ->
            case actual of
                [] ->
                    toString actual
                        ++ " has fewer items than"
                        ++ toString expected
                        |> Expect.fail

                ( actualX, actualY ) :: actualTail ->
                    let
                        context =
                            "when comparing\nexpected "
                                ++ toString ( expectedX, expectedY )
                                ++ "\n"
                                ++ "actual "
                                ++ toString ( actualX, actualY )
                    in
                    if isCloseEnough expectedX actualX then
                        if isCloseEnough expectedY actualY then
                            expectGraphDataToBeCloseTo expectedTail actualTail

                        else
                            context
                                ++ "\n"
                                ++ toString actualY
                                ++ " is not close enough to "
                                ++ toString expectedY
                                |> Expect.fail

                    else
                        context
                            ++ "\n"
                            ++ toString actualX
                            ++ " is not close enough to "
                            ++ toString expectedX
                            |> Expect.fail


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
                    |> expectGraphDataToBeCloseTo
                        [ ( 0, 17920.8 )
                        , ( 50, 19089.55 )
                        , ( 100, 20258.3 )
                        , ( 150, 21427.05 )
                        , ( 200, 22595.81 )
                        , ( 250, 23764.56 )
                        , ( 300, 24933.31 )
                        , ( 350, 26102.07 )
                        , ( 400, 27270.82 )
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
