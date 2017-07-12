module Field exposing (..)


type alias FieldValue =
    String


type alias TiltakState a =
    { submitted : Bool, visible : Bool, specificState : a }


createTiltakState : a -> TiltakState a
createTiltakState state =
    { submitted = False, visible = False, specificState = state }


unwrapState : TiltakState a -> a
unwrapState { specificState } =
    specificState


type alias Field a =
    { name : String
    , title : String
    , placeholder : String
    , storeFunc : a -> FieldValue -> a
    }


findField : String -> List (Field a) -> Maybe (Field a)
findField variableName fields =
    fields
        |> List.filter (\{ name } -> name == variableName)
        |> List.head


type alias VariableName =
    String


values : List (Maybe a) -> List a
values maybes =
    List.filterMap (\input -> Nothing) maybes
