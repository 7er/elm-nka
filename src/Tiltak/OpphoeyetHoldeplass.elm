module Tiltak.OpphoeyetHoldeplass exposing (..)

import Focus exposing (Focus, (=>))
import Tiltak exposing (Tiltak(..), StateCalculationMethod, sendTo)
import Field exposing (Field, SimpleField)
import TiltakStates
    exposing
        ( TiltakStates
        , OpphoeyetHoldeplassState
        , formattedValueDefault
        , installationCost
        , yearlyMaintenance
        , passengersPerYear
        , value
        )
import BasicTiltak
import GeneralForutsetninger


specificState : Focus TiltakStates OpphoeyetHoldeplassState
specificState =
    Focus.create
        .opphoeyetHoldeplass
        (\f tiltakStates ->
            { tiltakStates
                | opphoeyetHoldeplass = f tiltakStates.opphoeyetHoldeplass
            }
        )


yearlyPassasjerNytte : StateCalculationMethod
yearlyPassasjerNytte this ({ opphoeyetHoldeplass } as state) =
    let
        verdisettinger =
            GeneralForutsetninger.verdisettinger

        firstCalc passengersPerYear =
            passengersPerYear * verdisettinger.opphoyetHoldeplass

        first =
            Maybe.map firstCalc opphoeyetHoldeplass.passengersPerYear.value

        secondCalc beleggForbiPassasjererPerBuss aarligTidsbesparelseMinutter =
            beleggForbiPassasjererPerBuss
                * aarligTidsbesparelseMinutter
                * verdisettinger.reisetidKollektivTransport

        second =
            Maybe.map2
                secondCalc
                opphoeyetHoldeplass.beleggForbiPassasjererPerBuss.value
                opphoeyetHoldeplass.aarligTidsbesparelseMinutter.value
    in
        Maybe.map2 (+) first second


yearlyOperatoerNytte : StateCalculationMethod
yearlyOperatoerNytte this ({ opphoeyetHoldeplass } as state) =
    let
        verdisettinger =
            GeneralForutsetninger.verdisettinger
    in
        Maybe.map (\minutter -> minutter * verdisettinger.operatoerKostnad) opphoeyetHoldeplass.aarligTidsbesparelseMinutter.value


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


initialState : OpphoeyetHoldeplassState
initialState =
    { installationCost = formattedValueDefault
    , yearlyMaintenance = formattedValueDefault
    , bompengeAndel = 0
    , passengersPerYear = formattedValueDefault
    , beleggForbiPassasjererPerBuss = formattedValueDefault
    , aarligTidsbesparelseMinutter = formattedValueDefault
    , preferredToGraph = ""
    }


fieldDefinitions : List (SimpleField OpphoeyetHoldeplassState)
fieldDefinitions =
    let
        beleggForbiPassasjererPerBuss =
            Focus.create .beleggForbiPassasjererPerBuss
                (\f specificState ->
                    { specificState
                        | beleggForbiPassasjererPerBuss = f specificState.beleggForbiPassasjererPerBuss
                    }
                )

        beleggForbiPassasjererPerBussValue =
            beleggForbiPassasjererPerBuss => value

        aarligTidsbesparelseMinutter =
            Focus.create
                .aarligTidsbesparelseMinutter
                (\f specificState ->
                    { specificState
                        | aarligTidsbesparelseMinutter = f specificState.aarligTidsbesparelseMinutter
                    }
                )
    in
        [ { name = "installationCost"
          , title = "Installasjonskostnad"
          , placeholder = "Kostnaden ved å installere tiltaket en gang, kroner"
          , setter = Focus.set (installationCost => value)
          , accessor = Focus.get (installationCost => value)
          , stepSize = 50000
          }
        , { name = "yearlyMaintenance"
          , title = "Årlige drifts- og vedlikeholdskostnader"
          , placeholder = "Årlige drifts- og vedlikeholdskostnader, kroner"
          , setter = Focus.set (yearlyMaintenance => value)
          , accessor = Focus.get (yearlyMaintenance => value)
          , stepSize = 5000
          }
        , { name = "passengersPerYear"
          , title = "Antall av- og påstigende passasjerer på holdeplassen"
          , placeholder = "På- og avstigende passasjerer per år"
          , setter = Focus.set (passengersPerYear => value)
          , accessor = Focus.get (passengersPerYear => value)
          , stepSize = 50
          }
        , { name = "beleggForbiPassasjererPerBuss"
          , title = "Gjennomsnittsbelegg forbi holdeplassen"
          , placeholder = "Passasjerer pr buss"
          , setter = Focus.set beleggForbiPassasjererPerBussValue
          , accessor = Focus.get beleggForbiPassasjererPerBussValue
          , stepSize = 5
          }
        , { name = "aarligTidsbesparelseMinutter"
          , title = "Årlig tidsbesparelse ved raskere på- og avstigning, minutter"
          , placeholder = "Se forklarende tekst i rapport"
          , setter = Focus.set (aarligTidsbesparelseMinutter => value)
          , accessor = Focus.get (aarligTidsbesparelseMinutter => value)
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
