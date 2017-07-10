module Tiltak exposing (..)

import Dict exposing (Dict)
import GeneralForutsetninger


type VariableToGraph
    = TripsPerYear
    | YearlyMaintenance
    | InstallationCost


type alias TiltakForutsetninger =
    Dict String Float


tripsPerYearNettoNytteNullpunkt : TiltakForutsetninger -> Maybe Float
tripsPerYearNettoNytteNullpunkt model =
    let
        forutsetningerCopy =
            Dict.insert "tripsPerYear" 1 model
    in
        Maybe.map2 (\a b -> a / b / GeneralForutsetninger.afaktorVekst)
            (kost forutsetningerCopy)
            (yearlyNytte forutsetningerCopy)


nettoNytteNullPunkt : VariableToGraph -> TiltakForutsetninger -> Maybe Float
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


yearlyHelsegevinstOgEndretUlykkesrisiko : Maybe Float -> Maybe Float
yearlyHelsegevinstOgEndretUlykkesrisiko tripsPerYear =
    Maybe.map
        (\trips ->
            trips
                * usageIncrease
                * GeneralForutsetninger.avgTripLengthKm
                * GeneralForutsetninger.helsegevinstOgEndretUlykkesrisikoNOKPerKm
        )
        tripsPerYear


yearlyKoreduksjonSlitasjeDrift : Maybe Float -> Maybe Float
yearlyKoreduksjonSlitasjeDrift tripsPerYear =
    tripsPerYear
        |> Maybe.map
            (\trips ->
                trips
                    * usageIncrease
                    * GeneralForutsetninger.avgTripLengthKm
                    * GeneralForutsetninger.koreduksjonSlitasjeDriftNOKPerKm
            )


tripsPerYearEffects : Maybe Float -> Maybe Float
tripsPerYearEffects tripsPerYear =
    Maybe.map3 (\a b c -> a + b + c)
        (yearlyHelsegevinstOgEndretUlykkesrisiko tripsPerYear)
        (yearlyMiljoOgKlimaeffekt tripsPerYear)
        (yearlyKoreduksjonSlitasjeDrift tripsPerYear)


parkeringSyklistNytte : Maybe Float -> Maybe Float
parkeringSyklistNytte tripsPerYear =
    Maybe.map (\trips -> trips * nytteMultiplier) tripsPerYear


brukerNytte : TiltakForutsetninger -> Maybe Float
brukerNytte forutsetninger =
    yearlySyklistNytte forutsetninger |> Maybe.map ((*) GeneralForutsetninger.afaktorVekst)


yearlySyklistNytte : TiltakForutsetninger -> Maybe Float
yearlySyklistNytte forutsetninger =
    Maybe.map2 (+)
        (parkeringSyklistNytte <| Dict.get "tripsPerYear" forutsetninger)
        (Maybe.map (\nytte -> 0.5 * nytte * usageIncrease) <| parkeringSyklistNytte <| Dict.get "tripsPerYear" forutsetninger)


yearlyMiljoOgKlimaeffekt : Maybe Float -> Maybe Float
yearlyMiljoOgKlimaeffekt tripsPerYear =
    tripsPerYear
        |> Maybe.map
            (\trips ->
                trips
                    * usageIncrease
                    * GeneralForutsetninger.avgTripLengthKm
                    * GeneralForutsetninger.miljoOgKlimaeffektNOKPerKm
            )


yearlyNytte : TiltakForutsetninger -> Maybe Float
yearlyNytte forutsetninger =
    Maybe.map2 (+) (yearlySyklistNytte forutsetninger) (tripsPerYearEffects <| Dict.get "tripsPerYear" forutsetninger)


nytte : TiltakForutsetninger -> Maybe Float
nytte model =
    yearlyNytte model |> Maybe.map ((*) GeneralForutsetninger.afaktorVekst)


totalCostNowValue : TiltakForutsetninger -> Maybe Float
totalCostNowValue forutsetninger =
    Maybe.map2 (+)
        (investmentKostInklRestverdiValueToday <| Dict.get "installationCost" forutsetninger)
        (maintenanceCost <| Dict.get "yearlyMaintenance" forutsetninger)


kostUtenSkyggepris : TiltakForutsetninger -> Maybe Float
kostUtenSkyggepris =
    totalCostNowValue


maintenanceCost : Maybe Float -> Maybe Float
maintenanceCost yearlyMaintenance =
    yearlyMaintenance |> Maybe.map ((*) GeneralForutsetninger.afaktor)


kost : TiltakForutsetninger -> Maybe Float
kost forutsetninger =
    totalCostNowValue forutsetninger |> Maybe.map ((*) (1 + GeneralForutsetninger.skyggepris))


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


type alias Tiltak =
    { nytte : TiltakForutsetninger -> Maybe Float
    , kost : TiltakForutsetninger -> Maybe Float
    , totalCostNowValue : TiltakForutsetninger -> Maybe Float
    , nettoNytte : TiltakForutsetninger -> Maybe Float
    }


sykkelparkeringUte : Tiltak
sykkelparkeringUte =
    { nytte = nytte
    , kost = kost
    , totalCostNowValue = totalCostNowValue
    , nettoNytte = \forutsetninger -> Maybe.map2 (-) (nytte forutsetninger) (kost forutsetninger)
    }


type alias Basic =
    { sum : () -> Float
    , first : Float
    , second : Float
    }


base : Basic
base =
    let
        this =
            { sum = \() -> this.first + this.second
            , first = 4
            , second = 3
            }
    in
        this


derived =
    let
        super =
            base
    in
        { super | first = 1, second = 2 }
