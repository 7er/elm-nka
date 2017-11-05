module SimpleTiltak exposing (..)

import TiltakStates
    exposing
        ( SimpleCommonState
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
    { getter : TiltakStates.TiltakStates -> SimpleCommonState
    , levetid : Float
    , nytteMultiplikator : Float
    , stateMap : TiltakStates.StateMap SimpleCommonState
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


fieldDefinitions : List (SimpleField TiltakStates.SimpleCommonState)
fieldDefinitions =
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
      , title = "Antall passasjerer per år"
      , placeholder = "Påstigende passasjerer per år"
      , setter = Focus.set (passengersPerYear => value)
      , accessor = Focus.get (passengersPerYear => value)
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
                        Field.compileFields simpleTiltak.stateMap
                            simpleTiltak.getter
                            fieldDefinitions
                , skyggepris =
                    \this state ->
                        sendTo this .skyggeprisHelper state ((simpleTiltak.getter state).bompengeAndel)
                , yearlyPassasjerNytte =
                    \_ state ->
                        (simpleTiltak.getter state).passengersPerYear.value
                            |> Maybe.map ((*) simpleTiltak.nytteMultiplikator)
                , driftOgVedlihKost =
                    \_ state ->
                        BasicTiltak.driftOgVedlihKost (simpleTiltak.getter state)
                , investeringsKostInklRestverdi =
                    \_ state ->
                        BasicTiltak.investeringsKostInklRestverdi
                            (simpleTiltak.getter state)
                            simpleTiltak.levetid
            }
