module Tiltak.OpphoeyetHoldeplass exposing (..)

import Tiltak exposing (Tiltak(..), StateCalculationMethod, sendTo)
import Field exposing (Field, SimpleField)
import TiltakStates exposing (TiltakStates, OpphoyetHoldeplassState)
import BasicTiltak
import GeneralForutsetninger


yearlyPassasjerNytte : StateCalculationMethod
yearlyPassasjerNytte this ({ opphoeyetHoldeplass } as state) =
    let
        verdisettinger =
            GeneralForutsetninger.verdisettinger

        firstCalc passengersPerYear =
            passengersPerYear * verdisettinger.opphoyetHoldeplass

        first =
            Maybe.map firstCalc opphoeyetHoldeplass.passengersPerYear

        secondCalc beleggForbiPassasjererPerBuss aarligTidsbesparelseMinutter =
            beleggForbiPassasjererPerBuss
                * aarligTidsbesparelseMinutter
                * verdisettinger.reisetidKollektivTransport

        second =
            Maybe.map2
                secondCalc
                opphoeyetHoldeplass.beleggForbiPassasjererPerBuss
                opphoeyetHoldeplass.aarligTidsbesparelseMinutter
    in
        Maybe.map2 (+) first second


yearlyOperatoerNytte : StateCalculationMethod
yearlyOperatoerNytte this ({ opphoeyetHoldeplass } as state) =
    let
        verdisettinger =
            GeneralForutsetninger.verdisettinger
    in
        Maybe.map (\minutter -> minutter * verdisettinger.operatoerKostnad) opphoeyetHoldeplass.aarligTidsbesparelseMinutter


levetid : number
levetid =
    25


tiltak : Tiltak
tiltak =
    let
        basicTiltakRecord =
            BasicTiltak.basicTiltakRecord
    in
        Tiltak
            { basicTiltakRecord
                | title = \_ -> "Opphøyet holdeplass"
                , fields = \_ -> fields
                , yearlyPassasjerNytte = yearlyPassasjerNytte
                , yearlyOperatoerNytte = yearlyOperatoerNytte
                , investeringsKostInklRestverdi =
                    \_ { opphoeyetHoldeplass } ->
                        BasicTiltak.investeringsKostInklRestverdi
                            opphoeyetHoldeplass
                            levetid
                , driftOgVedlihKost =
                    \_ { opphoeyetHoldeplass } ->
                        BasicTiltak.driftOgVedlihKost opphoeyetHoldeplass
                , skyggepris =
                    \this ({ opphoeyetHoldeplass } as state) ->
                        sendTo
                            this
                            .skyggeprisHelper
                            state
                            opphoeyetHoldeplass.bompengeAndel
            }


initialState : OpphoyetHoldeplassState
initialState =
    { installationCost = Nothing
    , yearlyMaintenance = Nothing
    , bompengeAndel = 0
    , passengersPerYear = Nothing
    , beleggForbiPassasjererPerBuss = Nothing
    , aarligTidsbesparelseMinutter = Nothing
    , preferredToGraph = ""
    }


fieldDefinitions : List (SimpleField OpphoyetHoldeplassState)
fieldDefinitions =
    [ { name = "installationCost"
      , title = "Installasjonskostnad"
      , placeholder = "Kostnaden ved å installere tiltaket en gang, kroner"
      , setter =
            (\value state ->
                { state
                    | installationCost = value
                }
            )
      , accessor = .installationCost
      , stepSize = 50000
      }
    , { name = "yearlyMaintenance"
      , title = "Årlige drifts- og vedlikeholdskostnader"
      , placeholder = "Årlige drifts- og vedlikeholdskostnader, kroner"
      , setter =
            (\value state ->
                { state
                    | yearlyMaintenance = value
                }
            )
      , accessor = .yearlyMaintenance
      , stepSize = 5000
      }
    , { name = "passengersPerYear"
      , title = "Antall av- og påstigende passasjerer på holdeplassen"
      , placeholder = "På- og avstigende passasjerer per år"
      , setter =
            (\value state ->
                { state
                    | passengersPerYear = value
                }
            )
      , accessor = .passengersPerYear
      , stepSize = 50
      }
    , { name = "beleggForbiPassasjererPerBuss"
      , title = "Gjennomsnittsbelegg forbi holdeplassen"
      , placeholder = "Passasjerer pr buss"
      , setter =
            (\value state ->
                { state
                    | beleggForbiPassasjererPerBuss = value
                }
            )
      , accessor = .beleggForbiPassasjererPerBuss
      , stepSize = 5
      }
    , { name = "aarligTidsbesparelseMinutter"
      , title = "Årlig tidsbesparelse ved raskere på- og avstigning, minutter"
      , placeholder = "Se forklarende tekst i rapport"
      , setter =
            (\value state ->
                { state
                    | aarligTidsbesparelseMinutter = value
                }
            )
      , accessor = .aarligTidsbesparelseMinutter
      , stepSize = 1000
      }
    ]


fields : List Field
fields =
    let
        stateMap updater tiltakStates =
            { tiltakStates
                | opphoeyetHoldeplass = updater tiltakStates.opphoeyetHoldeplass
            }

        updateTiltakStateHelper =
            TiltakStates.stateUpdateHelper stateMap

        thisValueHelper =
            TiltakStates.valueHelper .opphoeyetHoldeplass
    in
        fieldDefinitions
            |> Field.transformToFields
                stateMap
                updateTiltakStateHelper
                thisValueHelper
