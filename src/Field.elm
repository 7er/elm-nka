module Field exposing (..)


type alias FieldValue =
    String


type alias FormState a =
    { a | submitted : Bool }


type alias Field a =
    { name : String
    , title : String
    , placeholder : String
    , storeFunc : FormState a -> FieldValue -> FormState a
    }


findField : String -> List (Field a) -> Maybe (Field a)
findField variableName fields =
    fields
        |> List.filter (\{ name } -> name == variableName)
        |> List.head


type alias VariableName =
    String
