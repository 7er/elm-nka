module OpphoeyetHoldeplassTest exposing (suite, tiltakSuiteTest)

import Expect exposing (Expectation)
import Test exposing (Test, describe, test, only, skip)
import TestSupport exposing (..)
import Tiltak exposing (TiltakAccessor, Tiltak, sendTo)
import TiltakStates exposing (TiltakStates)
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


checkNettoNytteNullpunkt : Tiltak -> TiltakStates -> Test
checkNettoNytteNullpunkt tiltak state =
    let
        makeTest field =
            test field.name <|
                \() ->
                    OpphoeyetHoldeplass.nettoNytteNullpunktFor tiltak state field
                        |> toString
                        |> Expect.equal (field.stringValueFromState state)
    in
        sendTo tiltak .fields |> List.map makeTest |> describe "Checking nettoNytteNullpunktFor"


suite : Test
suite =
    describe "OpphoeyetHoldeplass"
        [ describe "graphing"
            (let
                initialState =
                    TiltakAndGroupData.initialTiltakStates

                state =
                    { initialState
                        | opphoeyetHoldeplass =
                            { installationCost = Just 999994
                            , yearlyMaintenance = Just 860
                            , bompengeAndel = 0.0
                            , passengersPerYear = Just 6709
                            , beleggForbiPassasjererPerBuss =
                                Just 20
                            , aarligTidsbesparelseMinutter =
                                Just 2083
                            }
                    }

                maybeField =
                    sendTo tiltak .fields |> List.head

                field =
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
                , skip <|
                    test "findVariableToGraph" <|
                        \() ->
                            OpphoeyetHoldeplass.findVariableToGraph tiltak state
                                |> Expect.equal field
                , test "samples" <|
                    \() ->
                        OpphoeyetHoldeplass.samples tiltak state field
                            |> Expect.equal
                                [ 749994
                                , 799994
                                , 849994
                                , 899994
                                , 949994
                                , 999994
                                , 1049994
                                , 1099994
                                , 1149994
                                , 1199994
                                , 1249994
                                ]
                , checkNettoNytteNullpunkt tiltak state
                ]
            )
        ]
