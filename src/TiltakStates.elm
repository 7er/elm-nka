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


type alias KollektivPrioriteringLyskryssState =
    { installationCost : Maybe Float
    , yearlyMaintenance : Maybe Float
    , bompengeAndel : Float
    , passengersPerYear : Maybe Float
    , antallBilerForsinketPerAvgang : Maybe Float
    , forsinkelsePerBilSeconds : Maybe Float
    , antallPasserendeAvgangerPerYear : Maybe Float
    }


type alias OpphoyetHoldeplassState =
    { installationCost : Maybe Float
    , yearlyMaintenance : Maybe Float
    , bompengeAndel : Float
    , passengersPerYear : Maybe Float
    , beleggForbiPassasjererPerBuss : Maybe Float
    , aarligTidsbesparelseMinutter : Maybe Float
    }


type alias SimpleCommonState =
    { installationCost : Maybe Float
    , yearlyMaintenance : Maybe Float
    , passengersPerYear : Maybe Float
    , bompengeAndel : Float
    }


type alias TiltakStates =
    { {-
             sykkelParkeringUte : SykkelparkeringUteState
         , separatSykkelveg : SeparatSykkelvegState
      -}
      leskurUtenSitteplass : SimpleCommonState
    , leskurMedSitteplass : SimpleCommonState
    , skiltingIBuss : SimpleCommonState
    , kollektivPrioriteringLyskryss : KollektivPrioriteringLyskryssState
    , opphoeyetHoldeplass : OpphoyetHoldeplassState
    }


type alias Setter specificState =
    String -> specificState -> specificState


type alias StateMap specificState =
    (specificState -> specificState) -> TiltakStates -> TiltakStates


stateUpdateHelper :
    StateMap specificState
    -> Setter specificState
    -> String
    -> TiltakStates
    -> TiltakStates
stateUpdateHelper stateMap setter stringValue tiltakStates =
    tiltakStates
        |> stateMap (\specificState -> setter stringValue specificState)


stringValueHelper :
    (TiltakStates -> specificState)
    -> (specificState -> Maybe a)
    -> TiltakStates
    -> String
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
