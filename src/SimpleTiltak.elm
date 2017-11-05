module SimpleTiltak exposing (..)

import Focus exposing (..)
import TiltakStates
    exposing
        ( SimpleCommonState
        , TiltakStates
        , formattedValueDefault
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
    }


initialState : TiltakStates.SimpleCommonState
initialState =
    { installationCost = formattedValueDefault
    , yearlyMaintenance = formattedValueDefault
    , passengersPerYear = formattedValueDefault
    , bompengeAndel = 0
    , preferredToGraph = ""
    }


fieldDefinitions : Focus TiltakStates SimpleCommonState -> List (SimpleField TiltakStates.SimpleCommonState)
fieldDefinitions tiltakFocus =
    [ { name = "installationCost"
      , title = "Installasjonskostnad"
      , placeholder = "Kostnaden ved å installere tiltaket en gang, kroner"
      , setter = Focus.set (installationCost => value)
      , accessor = Focus.get (installationCost => value)
      , focus = tiltakFocus => installationCost
      , stepSize = 50000
      }
    , { name = "yearlyMaintenance"
      , title = "Årlige drifts- og vedlikeholdskostnader"
      , placeholder = "Årlige drifts- og vedlikeholdskostnader, kroner"
      , setter = Focus.set (yearlyMaintenance => value)
      , accessor = Focus.get (yearlyMaintenance => value)
      , focus = tiltakFocus => yearlyMaintenance
      , stepSize = 5000
      }
    , { name = "passengersPerYear"
      , title = "Antall passasjerer per år"
      , placeholder = "Påstigende passasjerer per år"
      , setter = Focus.set (passengersPerYear => value)
      , accessor = Focus.get (passengersPerYear => value)
      , focus = tiltakFocus => passengersPerYear
      , stepSize = 50
      }
    ]


createTiltak : SimpleTiltak -> Tiltak
createTiltak simpleTiltak =
    let
        basicTiltakRecord =
            BasicTiltak.basicTiltakRecord
    in
        Tiltak
            { basicTiltakRecord
                | title = \_ -> simpleTiltak.title
                , fields =
                    \_ ->
                        Field.compileFields
                            (Focus.update simpleTiltak.focus)
                            (Focus.get simpleTiltak.focus)
                            (fieldDefinitions simpleTiltak.focus)
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
