module Tiltak.LeskurMedSitteplass exposing (tiltak, initialState)

import GeneralForutsetninger
import Tiltak exposing (Tiltak(..), Field, StateCalculationMethod, SimpleField, bindTiltak, sendTo)
import TiltakStates exposing (TiltakStates)
import Tiltak.BasicTiltak as BasicTiltak


levetid : number
levetid =
    12


yearlyPassasjerNytte : StateCalculationMethod
yearlyPassasjerNytte this ({ leskurMedSitteplass } as state) =
    leskurMedSitteplass.passengersPerYear
        |> Maybe.map
            ((*) GeneralForutsetninger.leskurPaaBussholdeplassenMedSitteplassNOK)


investeringsKostInklRestverdi : StateCalculationMethod
investeringsKostInklRestverdi this ({ leskurMedSitteplass } as state) =
    BasicTiltak.investeringsKostInklRestverdi
        leskurMedSitteplass
        levetid


driftOgVedlihKost : StateCalculationMethod
driftOgVedlihKost this ({ leskurMedSitteplass } as state) =
    BasicTiltak.driftOgVedlihKost leskurMedSitteplass


skyggepris : StateCalculationMethod
skyggepris this ({ leskurMedSitteplass } as state) =
    sendTo this .skyggeprisHelper state leskurMedSitteplass.bompengeAndel


tiltak : Tiltak
tiltak =
    let
        basicTiltakRecord =
            BasicTiltak.basicTiltakRecord
    in
        Tiltak
            { basicTiltakRecord
                | title = \_ -> "Leskur med sitteplass"
                , fields = \_ -> fields
                , skyggepris = skyggepris
                , yearlyPassasjerNytte = yearlyPassasjerNytte
                , driftOgVedlihKost = driftOgVedlihKost
                , investeringsKostInklRestverdi = investeringsKostInklRestverdi
            }


initialState : TiltakStates.SimpleCommonState
initialState =
    { installationCost = Nothing
    , yearlyMaintenance = Nothing
    , passengersPerYear = Nothing
    , bompengeAndel = 0
    }


fieldDefinitions : List (SimpleField TiltakStates.SimpleCommonState)
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
      , title = "Antall passasjerer per år"
      , placeholder = "Påstigende passasjerer per år"
      , setter =
            (\value state ->
                { state
                    | passengersPerYear =
                        value
                }
            )
      , accessor = .passengersPerYear
      , stepSize = 50
      }
    ]


fields : List Field
fields =
    let
        stateMap func tiltakStates =
            { tiltakStates
                | leskurMedSitteplass = func tiltakStates.leskurMedSitteplass
            }

        updateTiltakStateHelper =
            TiltakStates.stateUpdateHelper stateMap

        thisStringValueHelper =
            TiltakStates.stringValueHelper .leskurMedSitteplass

        thisValueHelper =
            TiltakStates.valueHelper .leskurMedSitteplass
    in
        fieldDefinitions
            |> Tiltak.transformToFields
                stateMap
                updateTiltakStateHelper
                thisStringValueHelper
                thisValueHelper
