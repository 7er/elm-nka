module LeskurMedSitteplassTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (Test, describe, test)
import TestSupport exposing (..)
import GeneralForutsetninger
import Tiltak
import FormattedValue exposing (formattedValue)
import TiltakAndGroupData
import Models exposing (Group(Holdeplasser))


suite : Test
suite =
    let
        initialState =
            TiltakAndGroupData.initialTiltakStates

        state =
            { initialState
                | leskurMedSitteplass =
                    { installationCost = Just 100 |> formattedValue
                    , yearlyMaintenance = Just 200 |> formattedValue
                    , passengersPerYear = Just 30 |> formattedValue
                    , bompengeAndel = 0.2
                    , preferredToGraph = ""
                    }
            }

        holdeplassTiltak =
            TiltakAndGroupData.tiltakForGroup Holdeplasser

        tiltak =
            holdeplassTiltak
                |> List.filter (\tiltak -> (Tiltak.sendTo tiltak .title) == "Leskur med sitteplass")
                |> List.head
                |> \maybeTiltak ->
                    case maybeTiltak of
                        Just tiltak ->
                            tiltak

                        Nothing ->
                            Debug.crash "fant ikke tiltaket"

        checkWithState description accessor expectation =
            test description <|
                \() ->
                    Tiltak.sendTo tiltak accessor state
                        |> checkMaybe expectation
    in
        describe "LeskurMedSitteplass"
            [ describe "nytte calculcations"
                [ checkWithState
                    "passasjerNytte"
                    .passasjerNytte
                    (closeTo 6796.75 2)
                , checkWithState
                    "yearlyPassasjerNytte"
                    .yearlyPassasjerNytte
                    (closeTo 302.4 2)
                , checkWithState
                    "nytte"
                    .nytte
                    (closeTo 6796.75 2)
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
                , checkWithState
                    "skyggepris"
                    .skyggepris
                    (closeTo -667.28 2)
                ]
            , describe "nettonytte calculations"
                [ checkWithState
                    "nettoNytte"
                    .nettoNytte
                    (closeTo 1958.96 2)
                ]
            , describe
                "bompengeAndel"
                [ test "updateBompengeAndel True" <|
                    \() ->
                        let
                            newState =
                                Tiltak.updateBompengeAndel tiltak True state
                        in
                            newState.leskurMedSitteplass.bompengeAndel |> Expect.equal 1
                , test "updateBompengeAndel False" <|
                    \() ->
                        let
                            newState =
                                Tiltak.updateBompengeAndel tiltak False state
                        in
                            newState.leskurMedSitteplass.bompengeAndel |> Expect.equal 0
                ]
            ]
