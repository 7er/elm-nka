module Tiltak.Belysning exposing (..)

import GeneralForutsetninger exposing (verdisettinger)
import Tiltak exposing (Tiltak(..), StateCalculationMethod, bindTiltak, sendTo)
import Field exposing (Field, SimpleField)
import TiltakStates exposing (TiltakStates)
import Tiltak.BasicTiltak as BasicTiltak


levetid : number
levetid =
    20


yearlyPassasjerNytte : StateCalculationMethod
yearlyPassasjerNytte this ({ belysning } as state) =
    belysning.passengersPerYear
        |> Maybe.map
            ((*) verdisettinger.belysning)


investeringsKostInklRestverdi : StateCalculationMethod
investeringsKostInklRestverdi this ({ belysning } as state) =
    BasicTiltak.investeringsKostInklRestverdi
        belysning
        levetid


skyggepris : StateCalculationMethod
skyggepris this ({ belysning } as state) =
    sendTo this .skyggeprisHelper state belysning.bompengeAndel


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
                | belysning = func tiltakStates.belysning
            }

        updateTiltakStateHelper =
            TiltakStates.stateUpdateHelper stateMap

        thisValueHelper =
            TiltakStates.valueHelper .belysning
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
                | title = \_ -> "Belysning på holdeplass"
                , fields = \_ -> fields
                , skyggepris = skyggepris
                , yearlyPassasjerNytte = yearlyPassasjerNytte
                , driftOgVedlihKost = \_ { belysning } -> BasicTiltak.driftOgVedlihKost belysning
                , investeringsKostInklRestverdi = investeringsKostInklRestverdi
            }
