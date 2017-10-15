module NumberFormat exposing (..)

import FormatNumber
import FormatNumber.Locales as Locales


norwegianLocale : Locales.Locale
norwegianLocale =
    { decimals = 3
    , thousandSeparator = " "
    , decimalSeparator = ","
    , negativePrefix = "-"
    , negativeSuffix = ""
    }


pretty : Float -> String
pretty value =
    FormatNumber.format { norwegianLocale | decimals = 0 } value


prettyTwoDecimals : Float -> String
prettyTwoDecimals value =
    FormatNumber.format { norwegianLocale | decimals = 2 } value


maybePretty : Maybe Float -> String
maybePretty maybeValue =
    maybeValue
        |> Maybe.map pretty
        |> Maybe.withDefault "Ugyldig kalkulasjon"


maybePrettyTwoDecimals : Maybe Float -> String
maybePrettyTwoDecimals maybeValue =
    maybeValue
        |> Maybe.map prettyTwoDecimals
        |> Maybe.withDefault "Ugyldig kalkulasjon"
