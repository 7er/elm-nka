module ElmTests exposing (..)


valuesFromScratch : List (Maybe a) -> List a
valuesFromScratch maybes =
    let
        helper maybe accumulator =
            case maybe of
                Nothing ->
                    accumulator

                Just value ->
                    value :: accumulator
    in
        List.foldr helper [] maybes


values : List (Maybe a) -> List a
values =
    List.filterMap identity


type Silly a b
    = AsA a
    | AsB b
    | AlsoAsA a
    | DeeplySilly (Silly a a) (Silly Int b)
