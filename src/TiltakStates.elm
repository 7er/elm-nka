module TiltakStates exposing (..)


type alias KollektivPrioriteringLyskryssState =
    { installationCost : Maybe Float
    , yearlyMaintenance : Maybe Float
    , bompengeAndel : Float
    , passengersPerYear : Maybe Float
    , antallBilerForsinketPerAvgang : Maybe Float
    , forsinkelsePerBilSeconds : Maybe Float
    , antallPasserendeAvgangerPerYear : Maybe Float
    , preferredToGraph : String
    }


type alias KollektivPrioriteringSkiltingState =
    { installationCost : Maybe Float
    , yearlyMaintenance : Maybe Float
    , passengersPerYear : Maybe Float
    , bompengeAndel : Float
    , antallBilerForsinketPerYear : Maybe Float
    , forsinkelsePerBilSeconds : Maybe Float
    , antallBilerForkjoersrettPerYear : Maybe Float
    , tidsgevinstPerBilSeconds : Maybe Float
    , antallPasserendeAvgangerPerYear : Maybe Float
    , preferredToGraph : String
    }


type alias OpphoyetHoldeplassState =
    { installationCost : Maybe Float
    , yearlyMaintenance : Maybe Float
    , bompengeAndel : Float
    , passengersPerYear : Maybe Float
    , beleggForbiPassasjererPerBuss : Maybe Float
    , aarligTidsbesparelseMinutter : Maybe Float
    , preferredToGraph : String
    }


type alias SimpleCommonState =
    { installationCost : Maybe Float
    , yearlyMaintenance : Maybe Float
    , passengersPerYear : Maybe Float
    , bompengeAndel : Float
    , preferredToGraph : String
    }


type alias SuperSimpleCommonState =
    { yearlyMaintenance : Maybe Float
    , passengersPerYear : Maybe Float
    , bompengeAndel : Float
    , preferredToGraph : String
    }


type alias HplOppropState =
    { passengersPerYear : Maybe Float
    , preferredToGraph : String
    }


type alias BussrenholdState =
    { dailyCostPerBus : Maybe Float
    , numberOfBusesAffected : Maybe Float
    , bompengeAndel : Float
    , passengersPerYear : Maybe Float
    , preferredToGraph : String
    }


type alias TiltakStates =
    { leskurUtenSitteplass : SimpleCommonState
    , leskurMedSitteplass : SimpleCommonState
    , skiltingIBuss : SimpleCommonState
    , belysning : SimpleCommonState
    , sitteplassPaaHpl : SimpleCommonState
    , lokalkartPaaHpl : SimpleCommonState
    , rutekartPaaHpl : SimpleCommonState
    , pakkeSkiltOgOppropBuss : SimpleCommonState
    , destinasjonsSkiltPaaBuss : SimpleCommonState
    , avviksinformasjonHoeyttaler : SimpleCommonState
    , alarmsystemPaaHpl : SimpleCommonState
    , kollektivPrioriteringLyskryss : KollektivPrioriteringLyskryssState
    , opphoeyetHoldeplass : OpphoyetHoldeplassState
    , renholdPaaHpl : SuperSimpleCommonState
    , fjerningAvIsSnoePaaHpl : SuperSimpleCommonState
    , vektere : SuperSimpleCommonState
    , hplOpprop : HplOppropState
    , kollektivPrioriteringSkilting : KollektivPrioriteringSkiltingState
    , bussrenhold : BussrenholdState
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
