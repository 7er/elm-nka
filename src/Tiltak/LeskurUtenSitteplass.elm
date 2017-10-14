module Tiltak.LeskurUtenSitteplass exposing (..)

import GeneralForutsetninger exposing (verdisettinger)
import Tiltak exposing (Tiltak(..), StateCalculationMethod, bindTiltak, sendTo)
import Field exposing (Field, SimpleField)
import TiltakStates exposing (TiltakStates)
import Tiltak.BasicTiltak as BasicTiltak


levetid : number
levetid =
    12


yearlyPassasjerNytte : StateCalculationMethod
yearlyPassasjerNytte this ({ leskurUtenSitteplass } as state) =
    leskurUtenSitteplass.passengersPerYear
        |> Maybe.map
            ((*) verdisettinger.leskurPaaBussholdeplassenUtenSitteplass)


investeringsKostInklRestverdi : StateCalculationMethod
investeringsKostInklRestverdi this ({ leskurUtenSitteplass } as state) =
    BasicTiltak.investeringsKostInklRestverdi
        leskurUtenSitteplass
        levetid


driftOgVedlihKost : StateCalculationMethod
driftOgVedlihKost this ({ leskurUtenSitteplass } as state) =
    BasicTiltak.driftOgVedlihKost leskurUtenSitteplass


skyggepris : StateCalculationMethod
skyggepris this ({ leskurUtenSitteplass } as state) =
    sendTo this .skyggeprisHelper state leskurUtenSitteplass.bompengeAndel


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
                | leskurUtenSitteplass = func tiltakStates.leskurUtenSitteplass
            }

        updateTiltakStateHelper =
            TiltakStates.stateUpdateHelper stateMap

        thisValueHelper =
            TiltakStates.valueHelper .leskurUtenSitteplass
    in
        fieldDefinitions
            |> Field.transformToFields
                stateMap
                updateTiltakStateHelper
                thisValueHelper


tiltak : Tiltak
tiltak =
    let
        basicTiltakRecord =
            BasicTiltak.basicTiltakRecord
    in
        Tiltak
            { basicTiltakRecord
                | title = \_ -> "Leskur u sitteplass"
                , fields = \_ -> fields
                , skyggepris = skyggepris
                , yearlyPassasjerNytte = yearlyPassasjerNytte
                , driftOgVedlihKost = driftOgVedlihKost
                , investeringsKostInklRestverdi = investeringsKostInklRestverdi
            }
