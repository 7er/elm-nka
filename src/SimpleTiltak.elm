module SimpleTiltak exposing (..)

import Focus exposing (..)
import TiltakStates exposing (TiltakStates)
import SpecificStates exposing (SimpleCommonState)
import FormattedValue
    exposing
        ( formattedValueDefault
        , installationCost
        , value
        , yearlyMaintenance
        , passengersPerYear
        )
import Field exposing (SimpleField)
import BasicTiltak
import Tiltak exposing (Tiltak(..), StateCalculationMethod, bindTiltak, sendTo)
import Focus exposing ((=>))


type alias SimpleTiltak =
    { levetid : Float
    , nytteMultiplikator : Float
    , focus : Focus TiltakStates SimpleCommonState
    , title : String
    , isHoldeplassTiltak : Bool
    }


initialState : SimpleCommonState
initialState =
    { installationCost = formattedValueDefault
    , yearlyMaintenance = formattedValueDefault
    , passengersPerYear = formattedValueDefault
    , bompengeAndel = 0
    , preferredToGraph = ""
    }


fieldDefinitions : Focus TiltakStates SimpleCommonState -> Bool -> List SimpleField
fieldDefinitions tiltakFocus isHoldeplassTiltak =
    [ { name = "installationCost"
      , title = "Installasjonskostnad"
      , placeholder = "Kostnaden ved å installere tiltaket en gang, kroner"
      , focus = tiltakFocus => installationCost
      , stepSize = 50000
      }
    , { name = "yearlyMaintenance"
      , title = "Årlige drifts- og vedlikeholdskostnader"
      , placeholder = "Årlige drifts- og vedlikeholdskostnader, kroner"
      , focus = tiltakFocus => yearlyMaintenance
      , stepSize = 5000
      }
    , { name = "passengersPerYear"
      , title = "Antall passasjerer per år"
      , placeholder =
            case isHoldeplassTiltak of
                True ->
                    "Årlig antall påstigende passasjerer på holdeplassen"

                False ->
                    "Årlig antall passasjerer om bord"
      , focus = tiltakFocus => passengersPerYear
      , stepSize = 50
      }
    ]


createTiltak : SimpleTiltak -> Tiltak
createTiltak simpleTiltak =
    let
        basicTiltakRecord =
            BasicTiltak.basicTiltakRecord simpleTiltak.focus
    in
        Tiltak
            { basicTiltakRecord
                | title = \_ -> simpleTiltak.title
                , fields =
                    \_ ->
                        Field.transformToFields
                            (fieldDefinitions
                                simpleTiltak.focus
                                simpleTiltak.isHoldeplassTiltak
                            )
                , skyggepris =
                    \this state ->
                        sendTo this
                            .skyggeprisHelper
                            state
                            ((Focus.get simpleTiltak.focus state).bompengeAndel)
                , yearlyPassasjerNytte =
                    \_ state ->
                        state
                            |> Focus.get (simpleTiltak.focus => passengersPerYear => value)
                            |> Maybe.map ((*) simpleTiltak.nytteMultiplikator)
                , driftOgVedlihKost =
                    \_ state ->
                        state
                            |> Focus.get simpleTiltak.focus
                            |> BasicTiltak.driftOgVedlihKost
                , investeringsKostInklRestverdi =
                    \_ state ->
                        BasicTiltak.investeringsKostInklRestverdi
                            (Focus.get simpleTiltak.focus state)
                            simpleTiltak.levetid
            }
