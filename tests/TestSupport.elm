module TestSupport exposing (..)

import Expect exposing (Expectation)
import Test exposing (Test, describe, test, only, skip)
import Tiltak exposing (TiltakAccessor)
import TiltakStates exposing (TiltakStates)


closeTo : Float -> Int -> Float -> Expectation
closeTo expected precision actual =
    if Tiltak.closeToEqual expected precision actual then
        Expect.pass
    else
        (toString actual)
            ++ " is not near enough to "
            ++ (toString expected)
            ++ " using "
            ++ (toString precision)
            ++ " digits of precision"
            |> Expect.fail


checkMaybe : (a -> Expectation) -> Maybe a -> Expectation
checkMaybe expectation maybeValue =
    maybeValue
        |> Maybe.map expectation
        |> Maybe.withDefault (Expect.fail <| "Got nothing")


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


tiltakSuite : CheckWithStateFunction -> ExpectedRecord -> Test
tiltakSuite checkWithState expectedRecord =
    Test.concat
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
