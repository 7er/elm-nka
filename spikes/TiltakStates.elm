module TiltakStates exposing (..)


type alias BarTiltakState =
    { x : Maybe Int, y : Maybe Int }


type alias FooTiltakState =
    { a : Maybe Float
    , b : Maybe Float
    }


type alias TiltakStates =
    { fooTiltak : FooTiltakState
    , barTiltak : BarTiltakState
    }


type alias Setter a =
    String -> a -> a


type alias StateMap a =
    (a -> a) -> TiltakStates -> TiltakStates


stateUpdateHelper : StateMap a -> Setter a -> String -> TiltakStates -> TiltakStates
stateUpdateHelper stateMap mapper stringValue tiltakStates =
    tiltakStates
        |> stateMap (\state -> mapper stringValue state)


stringValueHelper : (TiltakStates -> specificState) -> (specificState -> Maybe a) -> TiltakStates -> String
stringValueHelper getSpecificState func states =
    let
        maybeValue =
            getSpecificState states |> func
    in
        case maybeValue of
            Just value ->
                toString value

            Nothing ->
                ""
