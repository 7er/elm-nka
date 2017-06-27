module Sykkeltiltak exposing (..)

import GeneralForutsetninger


type alias SykkelParkeringUteTiltakModel =
    { tripsPerYear : Int
    , installationCost : Float
    , yearlyMaintenance : Int
    }


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
