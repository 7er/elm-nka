module TiltakComponents.SykkelparkeringUte exposing (..)

import Models exposing (..)
import TiltakStates exposing (SykkelparkeringUteTiltakModel)
import GeneralForutsetninger


stateMap func tiltakStates =
    { tiltakStates | sykkelParkeringUteTiltakState = func tiltakStates.sykkelParkeringUteTiltakState }


fields : List Field
fields =
    let
        updateTiltakStateHelper =
            TiltakStates.stateUpdateHelper stateMap

        thisStringValueHelper =
            TiltakStates.stringValueHelper .sykkelParkeringUteTiltakState
    in
        [ { name = "tripsPerYear"
          , title = "Antall sykkelreiser per år"
          , placeholder = "Sykkelreiser som bruker tiltaket"
          , updateTiltakState =
                updateTiltakStateHelper
                    (\stringValue state ->
                        { state
                            | tripsPerYear =
                                String.toInt stringValue |> Result.toMaybe
                        }
                    )
          , stringValueFromState = thisStringValueHelper .tripsPerYear
          }
        , { name = "installationCost"
          , title = "Installasjonskostnad"
          , placeholder = ""
          , updateTiltakState =
                updateTiltakStateHelper
                    (\stringValue state ->
                        { state
                            | installationCost =
                                String.toFloat stringValue |> Result.toMaybe
                        }
                    )
          , stringValueFromState = thisStringValueHelper .installationCost
          }
        , { name = "yearlyMaintenance"
          , title = "Årlige drifts- og vedlikeholdskostnader"
          , placeholder = "Kostnaden ved å installere tiltaket en gang, kroner"
          , updateTiltakState =
                updateTiltakStateHelper
                    (\stringValue state ->
                        { state
                            | yearlyMaintenance =
                                String.toFloat stringValue |> Result.toMaybe
                        }
                    )
          , stringValueFromState = thisStringValueHelper .yearlyMaintenance
          }
        ]


tiltak : Tiltak
tiltak =
    { title = "Sikker sykkelparkering ute"
    , fields = fields
    , brukerNytte = \{ sykkelParkeringUteTiltakState } -> brukerNytte sykkelParkeringUteTiltakState
    , kostUtenSkyggepris = \{ sykkelParkeringUteTiltakState } -> kostUtenSkyggepris sykkelParkeringUteTiltakState
    }


initialState : SykkelparkeringUteTiltakModel
initialState =
    { tripsPerYear = Nothing
    , yearlyMaintenance = Nothing
    , installationCost = Nothing
    }


c3GraphId : String
c3GraphId =
    "sykkelparkeringUteGraph"


loadGraph : Cmd msg
loadGraph =
    generateC3 c3GraphId


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


brukerNytte : SykkelparkeringUteTiltakModel -> Maybe Float
brukerNytte forutsetninger =
    yearlySyklistNytte forutsetninger |> Maybe.map ((*) GeneralForutsetninger.afaktorVekst)


yearlySyklistNytte : SykkelparkeringUteTiltakModel -> Maybe Float
yearlySyklistNytte forutsetninger =
    Maybe.map2 (+)
        (parkeringSyklistNytte forutsetninger.tripsPerYear)
        (Maybe.map (\nytte -> 0.5 * nytte * usageIncrease) <| parkeringSyklistNytte forutsetninger.tripsPerYear)


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


yearlyNytte : SykkelparkeringUteTiltakModel -> Maybe Float
yearlyNytte forutsetninger =
    Maybe.map2 (+) (yearlySyklistNytte forutsetninger) (tripsPerYearEffects forutsetninger.tripsPerYear)


nytte : SykkelparkeringUteTiltakModel -> Maybe Float
nytte model =
    yearlyNytte model |> Maybe.map ((*) GeneralForutsetninger.afaktorVekst)


totalCostNowValue : SykkelparkeringUteTiltakModel -> Maybe Float
totalCostNowValue forutsetninger =
    Maybe.map2 (+)
        (investmentKostInklRestverdiValueToday forutsetninger.installationCost)
        (maintenanceCost forutsetninger.yearlyMaintenance)


kostUtenSkyggepris : SykkelparkeringUteTiltakModel -> Maybe Float
kostUtenSkyggepris =
    totalCostNowValue


maintenanceCost : Maybe Float -> Maybe Float
maintenanceCost yearlyMaintenance =
    yearlyMaintenance |> Maybe.map ((*) GeneralForutsetninger.afaktor)


kost : SykkelparkeringUteTiltakModel -> Maybe Float
kost forutsetninger =
    totalCostNowValue forutsetninger |> Maybe.map ((*) (1 + GeneralForutsetninger.skyggepris))


nettoNytte : SykkelparkeringUteTiltakModel -> Maybe Float
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
