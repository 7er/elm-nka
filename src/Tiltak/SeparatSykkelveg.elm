module Tiltak.SeparatSykkelveg exposing (..)

import Tiltak exposing (Tiltak)
import Models exposing (..)
import TiltakStates exposing (SeparatSykkelvegTiltakModel, TiltakStates)
import GeneralForutsetninger


stateMap : (SeparatSykkelvegTiltakModel -> SeparatSykkelvegTiltakModel) -> TiltakStates -> TiltakStates
stateMap func tiltakStates =
    { tiltakStates | separatSykkelvegTiltakState = func tiltakStates.separatSykkelvegTiltakState }


fields =
    let
        updateTiltakStateHelper =
            TiltakStates.stateUpdateHelper stateMap

        thisStringValueHelper =
            TiltakStates.stringValueHelper .separatSykkelvegTiltakState
    in
        [ { name = "lengthKm"
          , title = "Sykkelveiens lengde"
          , placeholder = "Lengde"
          , updateTiltakState =
                updateTiltakStateHelper
                    (\stringValue state ->
                        { state
                            | lengthKm = String.toFloat stringValue |> Result.toMaybe
                        }
                    )
          , stringValueFromState = thisStringValueHelper .lengthKm
          }
        , { name = "tripsPerYear"
          , title = "Antall sykkelreiser per år"
          , placeholder = "Sykkelreiser som bruker tiltaket"
          , updateTiltakState =
                updateTiltakStateHelper
                    (\stringValue state ->
                        { state
                            | tripsPerYear = String.toInt stringValue |> Result.toMaybe
                        }
                    )
          , stringValueFromState = thisStringValueHelper .tripsPerYear
          }
        , { name = "minutesSaved"
          , title = "Minutter spart"
          , placeholder = "Blabla"
          , updateTiltakState =
                updateTiltakStateHelper
                    (\stringValue state ->
                        { state
                            | minutesSaved = String.toFloat stringValue |> Result.toMaybe
                        }
                    )
          , stringValueFromState = thisStringValueHelper .minutesSaved
          }
        , { name = "investmentCost"
          , title = "Investeringskostander"
          , placeholder = "Investeringskostnaden"
          , updateTiltakState =
                updateTiltakStateHelper
                    (\stringValue state ->
                        { state
                            | investmentCost = String.toFloat stringValue |> Result.toMaybe
                        }
                    )
          , stringValueFromState = thisStringValueHelper .investmentCost
          }
        ]


initialState : SeparatSykkelvegTiltakModel
initialState =
    { lengthKm = Nothing
    , tripsPerYear = Nothing
    , minutesSaved = Nothing
    , investmentCost = Nothing
    }


c3GraphId : String
c3GraphId =
    "sykkelparkeringUteGraph"


loadGraph : Cmd msg
loadGraph =
    generateC3 c3GraphId


tiltak : Tiltak
tiltak =
    { title = "Separat sykkelveg"
    , brukerNytte = \{ separatSykkelvegTiltakState } -> brukerNytte separatSykkelvegTiltakState
    , kostUtenSkyggepris = \{ separatSykkelvegTiltakState } -> kostUtenSkyggepris separatSykkelvegTiltakState
    , fields = fields
    }


nytteMultiplier : Float
nytteMultiplier =
    GeneralForutsetninger.sykkelParkeringUteNOK


usageIncrease : Float
usageIncrease =
    let
        { sykkelParkeringUte } =
            GeneralForutsetninger.usageIncrease
    in
        sykkelParkeringUte


yearlyHelsegevinstOgEndretUlykkesrisiko : Maybe Int -> Maybe Float
yearlyHelsegevinstOgEndretUlykkesrisiko tripsPerYear =
    Maybe.map
        (\trips ->
            (toFloat trips)
                * usageIncrease
                * GeneralForutsetninger.avgTripLengthKm
                * GeneralForutsetninger.helsegevinstOgEndretUlykkesrisikoNOKPerKm
        )
        tripsPerYear


yearlyKoreduksjonSlitasjeDrift : Maybe Int -> Maybe Float
yearlyKoreduksjonSlitasjeDrift tripsPerYear =
    tripsPerYear
        |> Maybe.map
            (\trips ->
                (toFloat trips)
                    * usageIncrease
                    * GeneralForutsetninger.avgTripLengthKm
                    * GeneralForutsetninger.koreduksjonSlitasjeDriftNOKPerKm
            )


tripsPerYearEffects : Maybe Int -> Maybe Float
tripsPerYearEffects tripsPerYear =
    Maybe.map3 (\a b c -> a + b + c)
        (yearlyHelsegevinstOgEndretUlykkesrisiko tripsPerYear)
        (yearlyMiljoOgKlimaeffekt tripsPerYear)
        (yearlyKoreduksjonSlitasjeDrift tripsPerYear)


parkeringSyklistNytte : Maybe Int -> Maybe Float
parkeringSyklistNytte tripsPerYear =
    Maybe.map (\trips -> (toFloat trips) * nytteMultiplier) tripsPerYear


brukerNytte : SeparatSykkelvegTiltakModel -> Maybe Float
brukerNytte forutsetninger =
    yearlySyklistNytte forutsetninger |> Maybe.map ((*) GeneralForutsetninger.afaktorVekst)


syklistNytteCalculation : Float -> Float -> Float -> Float
syklistNytteCalculation tripsPerYear lengthKm minutesSaved =
    tripsPerYear
        * (lengthKm
            * GeneralForutsetninger.wtpSeparatSykkelveg
            + minutesSaved
            * GeneralForutsetninger.nokPrMinPrSyklist
          )


yearlySyklistNytte : SeparatSykkelvegTiltakModel -> Maybe Float
yearlySyklistNytte forutsetninger =
    let
        usageIncrease =
            GeneralForutsetninger.usageIncrease
    in
        Maybe.map3
            (\tripsPerYear lengthKm minutesSaved ->
                (syklistNytteCalculation
                    (toFloat tripsPerYear)
                    lengthKm
                    minutesSaved
                )
                    + (0.5
                        * (syklistNytteCalculation
                            ((toFloat tripsPerYear) * usageIncrease.separatSykkelveg)
                            lengthKm
                            minutesSaved
                          )
                      )
            )
            forutsetninger.tripsPerYear
            forutsetninger.lengthKm
            forutsetninger.minutesSaved


yearlyMiljoOgKlimaeffekt : Maybe Int -> Maybe Float
yearlyMiljoOgKlimaeffekt tripsPerYear =
    tripsPerYear
        |> Maybe.map
            (\trips ->
                (toFloat trips)
                    * usageIncrease
                    * GeneralForutsetninger.avgTripLengthKm
                    * GeneralForutsetninger.miljoOgKlimaeffektNOKPerKm
            )


yearlyNytte : SeparatSykkelvegTiltakModel -> Maybe Float
yearlyNytte forutsetninger =
    Maybe.map2 (+) (yearlySyklistNytte forutsetninger) (tripsPerYearEffects forutsetninger.tripsPerYear)


nytte : SeparatSykkelvegTiltakModel -> Maybe Float
nytte model =
    yearlyNytte model |> Maybe.map ((*) GeneralForutsetninger.afaktorVekst)


kostUtenSkyggepris : SeparatSykkelvegTiltakModel -> Maybe Float
kostUtenSkyggepris forutsetninger =
    let
        func investmentCost =
            investmentCost * (1 + 0.07 * GeneralForutsetninger.afaktor)
    in
        Maybe.map func forutsetninger.investmentCost


maintenanceCost : Maybe Float -> Maybe Float
maintenanceCost yearlyMaintenance =
    yearlyMaintenance |> Maybe.map ((*) GeneralForutsetninger.afaktor)


kost : SeparatSykkelvegTiltakModel -> Maybe Float
kost forutsetninger =
    Maybe.map kostByInvestmentCost forutsetninger.investmentCost


kostByInvestmentCost : Float -> Float
kostByInvestmentCost invCost =
    invCost * (1 + GeneralForutsetninger.skyggepris) * (1 + 0.07 * GeneralForutsetninger.afaktor)


nettoNytte : SeparatSykkelvegTiltakModel -> Maybe Float
nettoNytte forutsetninger =
    Maybe.map2 (-) (nytte forutsetninger) (kost forutsetninger)


investmentKostInklRestverdiValueToday : Maybe Float -> Maybe Float
investmentKostInklRestverdiValueToday installationCost =
    installationCost |> Maybe.map ((*) investmentFactor)


levetid : number
levetid =
    10


investmentFactor : Float
investmentFactor =
    let
        beregningsTekniskMellomregning =
            toFloat <| (GeneralForutsetninger.analysePeriode // levetid) + 1

        ledd1 =
            (1 - ((1 + GeneralForutsetninger.drente) ^ ((negate levetid) * beregningsTekniskMellomregning)))
                / (1 - ((1 + GeneralForutsetninger.drente) ^ (negate levetid)))

        ledd2 =
            (GeneralForutsetninger.analysePeriode - (levetid * beregningsTekniskMellomregning))
                / (levetid * ((1 + GeneralForutsetninger.drente) ^ GeneralForutsetninger.analysePeriode))
    in
        ledd1 + ledd2
