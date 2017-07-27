module Tiltak.OpphoeyetHoldeplass exposing (tiltak, initialState)

import Tiltak exposing (Tiltak(..), Field, StateCalculationMethod, sendTo)
import TiltakStates exposing (OpphoyetHoldeplassState)
import Tiltak.BasicTiltak as BasicTiltak exposing (SimpleField)
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
      }
    , { name = "passengersPerYear"
      , title = "Antall passasjerer på holdeplassen"
      , placeholder = "På- og avstigende passasjerer per år"
      , setter =
            (\value state ->
                { state
                    | passengersPerYear = value
                }
            )
      , accessor = .passengersPerYear
      }
    , { name = "beleggForbiPassasjererPerBuss"
      , title = "Gjennomstsbelegg forbi holdeplassen"
      , placeholder = "Passasjerer pr buss"
      , setter =
            (\value state ->
                { state
                    | beleggForbiPassasjererPerBuss = value
                }
            )
      , accessor = .beleggForbiPassasjererPerBuss
      }
    , { name = "aarligTidsbesparelseMinutter"
      , title = "Årlig tidsbesparelse i minutter"
      , placeholder = "Se tekst i rapport for detaljer"
      , setter =
            (\value state ->
                { state
                    | aarligTidsbesparelseMinutter = value
                }
            )
      , accessor = .aarligTidsbesparelseMinutter
      }
    ]


fields : List Field
fields =
    let
        stateMap func tiltakStates =
            { tiltakStates
                | opphoeyetHoldeplass = func tiltakStates.opphoeyetHoldeplass
            }

        updateTiltakStateHelper =
            TiltakStates.stateUpdateHelper stateMap

        thisStringValueHelper =
            TiltakStates.stringValueHelper .opphoeyetHoldeplass

        toRealField simpleField =
            { name = simpleField.name
            , title = simpleField.title
            , placeholder = simpleField.placeholder
            , updateTiltakState =
                updateTiltakStateHelper
                    (\stringValue state ->
                        let
                            pipeline =
                                String.toFloat stringValue
                                    |> Result.toMaybe
                                    |> simpleField.setter
                        in
                            pipeline state
                    )
            , stringValueFromState = thisStringValueHelper simpleField.accessor
            }
    in
        fieldDefinitions
            |> List.map toRealField
