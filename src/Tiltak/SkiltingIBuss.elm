module Tiltak.SkiltingIBuss exposing (..)

import Tiltak exposing (Tiltak(..), StateCalculationMethod, bindTiltak, sendTo)
import Field exposing (Field, SimpleField)
import TiltakStates exposing (TiltakStates)
import GeneralForutsetninger exposing (verdisettinger)
import Tiltak.BasicTiltak as BasicTiltak


levetid : number
levetid =
    7


yearlyPassasjerNytte : StateCalculationMethod
yearlyPassasjerNytte this ({ skiltingIBuss } as state) =
    skiltingIBuss.passengersPerYear
        |> Maybe.map
            ((*) verdisettinger.skiltingIBuss)


investeringsKostInklRestverdi : StateCalculationMethod
investeringsKostInklRestverdi this ({ skiltingIBuss } as state) =
    BasicTiltak.investeringsKostInklRestverdi
        skiltingIBuss
        levetid


skyggepris : StateCalculationMethod
skyggepris this ({ skiltingIBuss } as state) =
    sendTo this .skyggeprisHelper state skiltingIBuss.bompengeAndel


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
                | skiltingIBuss = func tiltakStates.skiltingIBuss
            }

        updateTiltakStateHelper =
            TiltakStates.stateUpdateHelper stateMap

        thisValueHelper =
            TiltakStates.valueHelper .skiltingIBuss
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
                | title = \_ -> "Elektronisk skilting i bussen av neste holdeplass"
                , fields = \_ -> fields
                , skyggepris = skyggepris
                , yearlyPassasjerNytte = yearlyPassasjerNytte
                , driftOgVedlihKost = \_ { skiltingIBuss } -> BasicTiltak.driftOgVedlihKost skiltingIBuss
                , investeringsKostInklRestverdi = investeringsKostInklRestverdi
            }
