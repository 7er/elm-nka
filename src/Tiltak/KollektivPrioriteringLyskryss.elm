module Tiltak.KollektivPrioriteringLyskryss exposing (..)

import Tiltak exposing (Tiltak(..), StateCalculationMethod)
import Tiltak.BasicTiltak as BasicTiltak
import TiltakStates exposing (KollektivPrioriteringLyskryssState)
import GeneralForutsetninger


levetid =
    15


tidsbesparelsePerAvgangSeconds =
    20


yearlyPassasjerNytte : StateCalculationMethod
yearlyPassasjerNytte this ({ kollektivPrioriteringLyskryss } as state) =
    kollektivPrioriteringLyskryss.passengersPerYear
        |> Maybe.map
            (\passengersPerYear ->
                (tidsbesparelsePerAvgangSeconds / 60)
                    * GeneralForutsetninger.reisetidKollektivTransportNOK
                    * passengersPerYear
            )


yearlyTrafikantNytte : StateCalculationMethod
yearlyTrafikantNytte this ({ kollektivPrioriteringLyskryss } as state) =
    Maybe.map3
        (\antallBilerForsinketPerAvgang antallPasserendeAvgangerPerYear forsinkelsePerBilSeconds ->
            antallBilerForsinketPerAvgang
                * antallPasserendeAvgangerPerYear
                * (negate forsinkelsePerBilSeconds / 60)
                * GeneralForutsetninger.reisetidBilNOK
        )
        kollektivPrioriteringLyskryss.antallBilerForsinketPerAvgang
        kollektivPrioriteringLyskryss.antallPasserendeAvgangerPerYear
        kollektivPrioriteringLyskryss.forsinkelsePerBilSeconds


yearlyOperatoerNytte : StateCalculationMethod
yearlyOperatoerNytte this ({ kollektivPrioriteringLyskryss } as state) =
    let
        calculation passerendeAvganger =
            passerendeAvganger
                * (tidsbesparelsePerAvgangSeconds / 60)
                * GeneralForutsetninger.operatoerKostnadNOK
    in
        kollektivPrioriteringLyskryss.antallPasserendeAvgangerPerYear
            |> Maybe.map calculation


tiltak : Tiltak
tiltak =
    let
        basicTiltakRecord =
            BasicTiltak.basicTiltakRecord
    in
        Tiltak
            { basicTiltakRecord
                | title = \_ -> "Kollektiv prioritering lyskryss"
                , yearlyPassasjerNytte = yearlyPassasjerNytte
                , yearlyTrafikantNytte = yearlyTrafikantNytte
                , yearlyOperatoerNytte = yearlyOperatoerNytte
            }


initialState : KollektivPrioriteringLyskryssState
initialState =
    { installationCost = Nothing
    , yearlyMaintenance = Nothing
    , passengersPerYear = Nothing
    , bompengeAndel = 0
    , antallBilerForsinketPerAvgang = Nothing
    , forsinkelsePerBilSeconds = Nothing
    , antallPasserendeAvgangerPerYear = Nothing
    }
