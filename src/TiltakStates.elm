module TiltakStates exposing (..)


type alias SykkelparkeringUteState =
    { tripsPerYear : Maybe Int
    , installationCost : Maybe Float
    , yearlyMaintenance : Maybe Float
    }


type alias SeparatSykkelvegState =
    { lengthKm : Maybe Float
    , tripsPerYear : Maybe Int
    , minutesSaved : Maybe Float
    , investmentCost : Maybe Float
    }


type alias SimpleCommonState =
    { installationCost : Maybe Float
    , yearlyMaintenance : Maybe Float
    , passengersPerYear : Maybe Int
    }


type alias TiltakStates =
    { sykkelParkeringUte : SykkelparkeringUteState
    , separatSykkelveg : SeparatSykkelvegState
    , leskurUtenSitteplass : SimpleCommonState
    , leskurMedSitteplass : SimpleCommonState
    , skiltingIBuss : SimpleCommonState
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
