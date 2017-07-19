module TiltakStates exposing (..)


type alias SykkelparkeringUteTiltakModel =
    { tripsPerYear : Maybe Int
    , installationCost : Maybe Float
    , yearlyMaintenance : Maybe Float
    }


type alias SeparatSykkelvegTiltakModel =
    { lengthKm : Maybe Float
    , tripsPerYear : Maybe Int
    , minutesSaved : Maybe Float
    , investmentCost : Maybe Float
    }


type alias TiltakStates =
    { sykkelParkeringUteTiltakState : SykkelparkeringUteTiltakModel
    , separatSykkelvegTiltakState : SeparatSykkelvegTiltakModel
    , leskurUtenSitteplassTiltakState : {}
    , skiltingIBussTiltakState : {}
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
