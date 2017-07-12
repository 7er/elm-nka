module Field exposing (..)


type alias FieldValue =
    String


type alias TiltakState a =
    { a | submitted : Bool, visible : Bool }


type alias Field a =
    { name : String
    , title : String
    , placeholder : String
    , storeFunc : TiltakState a -> FieldValue -> TiltakState a
    }


findField : String -> List (Field a) -> Maybe (Field a)
findField variableName fields =
    fields
        |> List.filter (\{ name } -> name == variableName)
        |> List.head


type alias VariableName =
    String
