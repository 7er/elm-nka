module SykkelparkeringUteTiltak exposing (..)

import GeneralForutsetninger


type VariableToGraph
    = TripsPerYear
    | YearlyMaintenance
    | InstallationCost


type alias SykkelParkeringUteTiltakModel =
    { tripsPerYear : Int
    , installationCost : Float
    , yearlyMaintenance : Float
    }


tripsPerYearNettoNytteNullpunkt model =
    let
        forutsetningerCopy =
            { model | tripsPerYear = 1 }
    in
        (kost forutsetningerCopy)
            / (yearlyNytte forutsetningerCopy)
            / GeneralForutsetninger.afaktorVekst


nettoNytteNullPunkt : VariableToGraph -> SykkelParkeringUteTiltakModel -> Float
nettoNytteNullPunkt variable model =
    case variable of
        TripsPerYear ->
            tripsPerYearNettoNytteNullpunkt model

        _ ->
            0


schemaVariablesToGraph : List VariableToGraph
schemaVariablesToGraph =
    [ TripsPerYear, YearlyMaintenance, InstallationCost ]


nytteMultiplier =
    GeneralForutsetninger.sykkelParkeringUteNOK


usageIncrease =
    let
        { sykkelParkeringUte } =
            GeneralForutsetninger.usageIncrease
    in
        sykkelParkeringUte


yearlyHelsegevinstOgEndretUlykkesrisiko : Int -> Float
yearlyHelsegevinstOgEndretUlykkesrisiko tripsPerYear =
    (toFloat tripsPerYear)
        * usageIncrease
        * GeneralForutsetninger.avgTripLengthKm
        * GeneralForutsetninger.helsegevinstOgEndretUlykkesrisikoNOKPerKm


yearlyKoreduksjonSlitasjeDrift : Int -> Float
yearlyKoreduksjonSlitasjeDrift tripsPerYear =
    (toFloat tripsPerYear)
        * usageIncrease
        * GeneralForutsetninger.avgTripLengthKm
        * GeneralForutsetninger.koreduksjonSlitasjeDriftNOKPerKm


tripsPerYearEffects : Int -> Float
tripsPerYearEffects tripsPerYear =
    (yearlyHelsegevinstOgEndretUlykkesrisiko tripsPerYear)
        + (yearlyMiljoOgKlimaeffekt tripsPerYear)
        + (yearlyKoreduksjonSlitasjeDrift tripsPerYear)


parkeringSyklistNytte : Int -> Float
parkeringSyklistNytte tripsPerYear =
    (toFloat tripsPerYear) * nytteMultiplier


brukerNytte : SykkelParkeringUteTiltakModel -> Float
brukerNytte forutsetninger =
    (yearlySyklistNytte forutsetninger) * GeneralForutsetninger.afaktorVekst


yearlySyklistNytte forutsetninger =
    (parkeringSyklistNytte forutsetninger.tripsPerYear)
        + (0.5 * (parkeringSyklistNytte forutsetninger.tripsPerYear) * usageIncrease)


yearlyMiljoOgKlimaeffekt : Int -> Float
yearlyMiljoOgKlimaeffekt tripsPerYear =
    (toFloat tripsPerYear)
        * usageIncrease
        * GeneralForutsetninger.avgTripLengthKm
        * GeneralForutsetninger.miljoOgKlimaeffektNOKPerKm


yearlyNytte : SykkelParkeringUteTiltakModel -> Float
yearlyNytte forutsetninger =
    (yearlySyklistNytte forutsetninger) + (tripsPerYearEffects forutsetninger.tripsPerYear)


nytte : SykkelParkeringUteTiltakModel -> Float
nytte model =
    yearlyNytte model * GeneralForutsetninger.afaktorVekst


totalCostNowValue forutsetninger =
    investmentKostInklRestverdiValueToday forutsetninger.installationCost
        + maintenanceCost forutsetninger.yearlyMaintenance


maintenanceCost yearlyMaintenance =
    yearlyMaintenance * GeneralForutsetninger.afaktor


kost forutsetninger =
    let
        totalCostNow =
            totalCostNowValue forutsetninger
    in
        totalCostNow * (1 + GeneralForutsetninger.skyggepris)


nettoNytte forutsetninger =
    nytte forutsetninger - kost forutsetninger


investmentKostInklRestverdiValueToday : Float -> Float
investmentKostInklRestverdiValueToday installationCost =
    installationCost * investmentFactor


levetid =
    10


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
