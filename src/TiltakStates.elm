module TiltakStates exposing (..)


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
    { leskurUtenSitteplass : SimpleCommonState
    , leskurMedSitteplass : SimpleCommonState
    , skiltingIBuss : SimpleCommonState
    , belysning : SimpleCommonState
    , sitteplassPaaHpl : SimpleCommonState
    , lokalkartPaaHpl : SimpleCommonState
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


valueHelper :
    (TiltakStates -> specificState)
    -> (specificState -> Maybe a)
    -> TiltakStates
    -> Maybe a
valueHelper getSpecificState func states =
    getSpecificState states |> func
