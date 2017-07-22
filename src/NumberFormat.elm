module NumberFormat exposing (..)


prettyHelper : List String -> String -> String
prettyHelper acc rest =
    case rest of
        "" ->
            String.join " " acc

        _ ->
            let
                group =
                    String.right 3 rest

                remainder =
                    String.slice 0 -3 rest
            in
                prettyHelper (group :: acc) remainder


pretty : Float -> String
pretty value =
    case value >= 0 of
        True ->
            value |> round |> toString |> prettyHelper []

        False ->
            negate value |> pretty |> String.cons '-'


maybePretty : Maybe Float -> String
maybePretty maybeValue =
    case maybeValue of
        Just value ->
            pretty value

        Nothing ->
            "Ugyldig kalkulasjon"


prettyTwoDecimals : Float -> String
prettyTwoDecimals value =
    let
        wholePart =
            case value >= 0 of
                True ->
                    value |> truncate |> toString |> prettyHelper []

                False ->
                    negate value |> pretty |> String.cons '-'

        decimalsPart =
            value
                |> abs
                |> (*) 100
                |> round
                |> toString
                |> String.padRight 2 '0'
                |> String.right 2
    in
        wholePart ++ "," ++ decimalsPart


maybePrettyTwoDecimals : Maybe Float -> String
maybePrettyTwoDecimals maybeValue =
    maybeValue
        |> Maybe.map prettyTwoDecimals
        |> Maybe.withDefault "Ugyldig kalkulasjon"



{- case maybeValue of
   Just value ->
       prettyTwoDecimals value

   Nothing ->
       "Ugyldig kalkulasjon"
-}
