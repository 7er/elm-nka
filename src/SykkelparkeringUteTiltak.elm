module SykkelparkeringUteTiltak exposing (..)

import GeneralForutsetninger


type VariableToGraph
    = TripsPerYear
    | YearlyMaintenance
    | InstallationCost


type alias SykkelParkeringUteTiltakModel =
    { tripsPerYear : Maybe Int
    , installationCost : Maybe Float
    , yearlyMaintenance : Maybe Float
    }


tripsPerYearNettoNytteNullpunkt : SykkelParkeringUteTiltakModel -> Maybe Float
tripsPerYearNettoNytteNullpunkt model =
    let
        forutsetningerCopy =
            { model | tripsPerYear = Just 1 }
    in
        Maybe.map2 (\a b -> a / b / GeneralForutsetninger.afaktorVekst)
            (kost forutsetningerCopy)
            (yearlyNytte forutsetningerCopy)


nettoNytteNullPunkt : VariableToGraph -> SykkelParkeringUteTiltakModel -> Maybe Float
nettoNytteNullPunkt variable model =
    case variable of
        TripsPerYear ->
            tripsPerYearNettoNytteNullpunkt model

        _ ->
            Debug.crash "TODO"


schemaVariablesToGraph : List VariableToGraph
schemaVariablesToGraph =
    [ TripsPerYear, YearlyMaintenance, InstallationCost ]


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


brukerNytte : SykkelParkeringUteTiltakModel -> Maybe Float
brukerNytte forutsetninger =
    yearlySyklistNytte forutsetninger |> Maybe.map ((*) GeneralForutsetninger.afaktorVekst)


yearlySyklistNytte : SykkelParkeringUteTiltakModel -> Maybe Float
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


yearlyNytte : SykkelParkeringUteTiltakModel -> Maybe Float
yearlyNytte forutsetninger =
    Maybe.map2 (+) (yearlySyklistNytte forutsetninger) (tripsPerYearEffects forutsetninger.tripsPerYear)


nytte : SykkelParkeringUteTiltakModel -> Maybe Float
nytte model =
    yearlyNytte model |> Maybe.map ((*) GeneralForutsetninger.afaktorVekst)


totalCostNowValue : SykkelParkeringUteTiltakModel -> Maybe Float
totalCostNowValue forutsetninger =
    Maybe.map2 (+)
        (investmentKostInklRestverdiValueToday forutsetninger.installationCost)
        (maintenanceCost forutsetninger.yearlyMaintenance)


kostUtenSkyggepris : SykkelParkeringUteTiltakModel -> Maybe Float
kostUtenSkyggepris =
    totalCostNowValue


maintenanceCost : Maybe Float -> Maybe Float
maintenanceCost yearlyMaintenance =
    yearlyMaintenance |> Maybe.map ((*) GeneralForutsetninger.afaktor)


kost : SykkelParkeringUteTiltakModel -> Maybe Float
kost forutsetninger =
    totalCostNowValue forutsetninger |> Maybe.map ((*) (1 + GeneralForutsetninger.skyggepris))


nettoNytte : SykkelParkeringUteTiltakModel -> Maybe Float
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
