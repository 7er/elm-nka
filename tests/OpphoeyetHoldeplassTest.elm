module OpphoeyetHoldeplassTest exposing (suite)

import Expect exposing (Expectation)
import Test exposing (Test, describe, test, only, skip)
import TestSupport exposing (..)
import Tiltak exposing (TiltakAccessor)
import Tiltak.OpphoeyetHoldeplass as OpphoeyetHoldeplass
import TiltakAndGroupData
import TiltakStates exposing (TiltakStates)


type alias ExpectedRecord =
    { driftOgVedlihKost : Float
    , investeringsKostInklRestverdi : Float
    , kostUtenSkyggepris : Float
    , nettoNytte : Float
    , nytte : Float
    , operatoerNytte : Float
    , passasjerNytte : Float
    , skyggepris : Float
    , trafikantNytte : Float
    , yearlyOperatoerNytte : Float
    , yearlyPassasjerNytte : Float
    , yearlyTrafikantNytte : Float
    }


type alias CheckWithStateFunction =
    String -> TiltakAccessor (TiltakStates -> Maybe Float) -> (Float -> Expectation) -> Test


tiltakSuite : CheckWithStateFunction -> ExpectedRecord -> List Test
tiltakSuite checkWithState expectedRecord =
    [ describe "nytte calculcations"
        [ checkWithState
            "yearlyPassasjerNytte"
            .yearlyPassasjerNytte
            (closeTo expectedRecord.yearlyPassasjerNytte 2)
        , checkWithState
            "passasjerNytte"
            .passasjerNytte
            (closeTo expectedRecord.passasjerNytte 2)
        , checkWithState
            "yearlyTrafikantNytte"
            .yearlyTrafikantNytte
            (closeTo expectedRecord.yearlyTrafikantNytte 2)
        , checkWithState
            "trafikantNytte"
            .trafikantNytte
            (closeTo expectedRecord.trafikantNytte 2)
        , checkWithState
            "yearlyOperatoerNytte"
            .yearlyOperatoerNytte
            (closeTo expectedRecord.yearlyOperatoerNytte 2)
        , checkWithState
            "operatoerNytte"
            .operatoerNytte
            (closeTo expectedRecord.operatoerNytte 2)
        , checkWithState
            "nytte"
            .nytte
            (closeTo expectedRecord.nytte 2)
        ]
    , describe "kost calculations"
        [ checkWithState
            "investeringsKostInklRestverdi"
            .investeringsKostInklRestverdi
            (closeTo expectedRecord.investeringsKostInklRestverdi 2)
        , checkWithState
            "driftOgVedlihKost"
            .driftOgVedlihKost
            (closeTo expectedRecord.driftOgVedlihKost 2)
        , checkWithState
            "kostUtenSkyggepris"
            .kostUtenSkyggepris
            (closeTo expectedRecord.kostUtenSkyggepris 2)
        , checkWithState
            "skyggepris"
            .skyggepris
            (closeTo expectedRecord.skyggepris 2)
        ]
    , describe "nettonytte calculations"
        [ checkWithState
            "nettoNytte"
            .nettoNytte
            (closeTo expectedRecord.nettoNytte 2)
        ]
    ]


suite : Test
suite =
    describe "OpphoeyetHoldeplass" <|
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
                        , aarligAntallAvganger = Just 1000
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
                        Tiltak.sendTo
                            OpphoeyetHoldeplass.tiltak
                            accessor
                            state
                            |> checkMaybe expectation
        in
            tiltakSuite checkWithState expectedRecord
