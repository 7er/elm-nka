module NumberFormat exposing (pretty)


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
