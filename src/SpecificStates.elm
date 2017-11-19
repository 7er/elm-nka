module SpecificStates exposing (..)

import FormattedValue exposing (..)


type alias BasicState =
    { passengersPerYear : FormattedValue Float
    , preferredToGraph : String
    , bompengeAndel : Float -- spuriøs, ingen kostnader her
    }


type alias HplOppropState =
    BasicState


type alias SuperSimpleCommonPartial a =
    { a | yearlyMaintenance : FormattedValue Float }


type alias SuperSimpleCommonState =
    SuperSimpleCommonPartial BasicState


type alias SimpleCommonPartial a =
    { a
        | installationCost : FormattedValue Float
    }


type alias SimpleCommonState =
    SimpleCommonPartial SuperSimpleCommonState


type alias OpphoeyetHoldeplassStatePartial a =
    { a
        | beleggForbiPassasjererPerBuss : FormattedValue Float
        , aarligTidsbesparelseMinutter : FormattedValue Float
    }


type alias OpphoeyetHoldeplassState =
    OpphoeyetHoldeplassStatePartial SimpleCommonState


type alias BussrenholdState =
    { dailyCostPerBus : FormattedValue Float
    , numberOfBusesAffected : FormattedValue Float
    , bompengeAndel : Float
    , passengersPerYear : FormattedValue Float
    , preferredToGraph : String
    }


type alias LaventrebussStatePartial a =
    { a
        | passasjererPerBuss : FormattedValue Float
        , yearlyTidsbesparelseMinutter : FormattedValue Float
        , passasjererTilpassedeHoldplasserPercent : FormattedValue Float -- bruk Percent type
    }


type alias LaventrebussState =
    LaventrebussStatePartial SimpleCommonState


type alias KantsteinstoppPartial a =
    { a
        | antallBilerForsinketPerAvgang : FormattedValue Float
        , antallBussavgangerPerYear : FormattedValue Float
    }


type alias KantsteinstoppState =
    KantsteinstoppPartial SimpleCommonState


type alias KollektivPrioriteringLyskryssStatePartial a =
    { a
        | antallBilerForsinketPerAvgang : FormattedValue Float
        , forsinkelsePerBilSeconds : FormattedValue Float
        , antallPasserendeAvgangerPerYear : FormattedValue Float
    }


type alias KollektivPrioriteringLyskryssState =
    KollektivPrioriteringLyskryssStatePartial SimpleCommonState


type alias KollektivPrioriteringSkiltingPartial a =
    { a
        | antallBilerForsinketPerYear : FormattedValue Float
        , forsinkelsePerBilSeconds : FormattedValue Float
        , antallBilerForkjoersrettPerYear : FormattedValue Float
        , tidsgevinstPerBilSeconds : FormattedValue Float
        , antallPasserendeAvgangerPerYear : FormattedValue Float
    }


type alias KollektivPrioriteringSkiltingState =
    KollektivPrioriteringSkiltingPartial SimpleCommonState
