module TiltakStates exposing (..)

import Focus exposing (Focus, (=>))


type Editable
    = Edit
    | Display


type alias FormattedValue valueType =
    { value : Maybe valueType
    , state : Editable
    }


state =
    Focus.create
        .state
        (\f formattedValue ->
            { formattedValue | state = f formattedValue.state }
        )


formattedValueDefault : FormattedValue a
formattedValueDefault =
    { value = Nothing
    , state = Display
    }


formattedValue value =
    { value = value
    , state = Display
    }


value : Focus { formattedValue | value : Maybe a } (Maybe a)
value =
    Focus.create
        .value
        (\f formattedValue ->
            { formattedValue | value = f formattedValue.value }
        )


yearlyMaintenance : Focus { specificState | yearlyMaintenance : a } a
yearlyMaintenance =
    Focus.create
        .yearlyMaintenance
        (\f specificState ->
            { specificState | yearlyMaintenance = f specificState.yearlyMaintenance }
        )


passengersPerYear : Focus { specificState | passengersPerYear : a } a
passengersPerYear =
    Focus.create
        .passengersPerYear
        (\f specificState ->
            { specificState | passengersPerYear = f specificState.passengersPerYear }
        )


installationCost : Focus { specificState | installationCost : a } a
installationCost =
    Focus.create
        .installationCost
        (\f specificState ->
            { specificState | installationCost = f specificState.installationCost }
        )


installationCostValue =
    installationCost => value


type alias KollektivPrioriteringLyskryssState =
    { installationCost : FormattedValue Float
    , yearlyMaintenance : FormattedValue Float
    , bompengeAndel : Float
    , passengersPerYear : FormattedValue Float
    , antallBilerForsinketPerAvgang : FormattedValue Float
    , forsinkelsePerBilSeconds : FormattedValue Float
    , antallPasserendeAvgangerPerYear : FormattedValue Float
    , preferredToGraph : String
    }


type alias KollektivPrioriteringSkiltingState =
    { installationCost : FormattedValue Float
    , yearlyMaintenance : FormattedValue Float
    , passengersPerYear : FormattedValue Float
    , bompengeAndel : Float
    , antallBilerForsinketPerYear : FormattedValue Float
    , forsinkelsePerBilSeconds : FormattedValue Float
    , antallBilerForkjoersrettPerYear : FormattedValue Float
    , tidsgevinstPerBilSeconds : FormattedValue Float
    , antallPasserendeAvgangerPerYear : FormattedValue Float
    , preferredToGraph : String
    }


type alias OpphoeyetHoldeplassState =
    { installationCost : FormattedValue Float
    , yearlyMaintenance : FormattedValue Float
    , bompengeAndel : Float
    , passengersPerYear : FormattedValue Float
    , beleggForbiPassasjererPerBuss : FormattedValue Float
    , aarligTidsbesparelseMinutter : FormattedValue Float
    , preferredToGraph : String
    }


type alias SimpleCommonState =
    { installationCost : FormattedValue Float
    , yearlyMaintenance : FormattedValue Float
    , passengersPerYear : FormattedValue Float
    , bompengeAndel : Float
    , preferredToGraph : String
    }


type alias SuperSimpleCommonState =
    { yearlyMaintenance : FormattedValue Float
    , passengersPerYear : FormattedValue Float
    , bompengeAndel : Float
    , preferredToGraph : String
    }


type alias HplOppropState =
    { passengersPerYear : FormattedValue Float
    , preferredToGraph : String
    }


type alias BussrenholdState =
    { dailyCostPerBus : FormattedValue Float
    , numberOfBusesAffected : FormattedValue Float
    , bompengeAndel : Float
    , passengersPerYear : FormattedValue Float
    , preferredToGraph : String
    }


type alias LaventrebussState =
    { installationCost : FormattedValue Float
    , yearlyMaintenance : FormattedValue Float
    , bompengeAndel : Float
    , passengersPerYear : FormattedValue Float
    , passasjererPerBuss : FormattedValue Float
    , yearlyTidsbesparelseMinutter : FormattedValue Float
    , passasjererTilpassedeHoldplasserPercent : FormattedValue Float -- bruk Percent type
    , preferredToGraph : String
    }


type alias KantsteinstoppState =
    { installationCost : FormattedValue Float
    , yearlyMaintenance : FormattedValue Float
    , bompengeAndel : Float
    , passengersPerYear : FormattedValue Float
    , antallBilerForsinketPerAvgang : FormattedValue Float
    , antallBussavgangerPerYear : FormattedValue Float
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
    , opphoeyetHoldeplass : OpphoeyetHoldeplassState
    , renholdPaaHpl : SuperSimpleCommonState
    , fjerningAvIsSnoePaaHpl : SuperSimpleCommonState
    , vektere : SuperSimpleCommonState
    , hplOpprop : HplOppropState
    , kollektivPrioriteringSkilting : KollektivPrioriteringSkiltingState
    , bussrenhold : BussrenholdState
    , laventrebuss : LaventrebussState
    , kantsteinstopp : KantsteinstoppState
    }
