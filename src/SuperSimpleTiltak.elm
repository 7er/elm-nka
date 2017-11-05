module SuperSimpleTiltak exposing (..)

import Focus exposing (Focus, (=>))
import TiltakStates
    exposing
        ( SimpleCommonState
        , SuperSimpleCommonState
        , formattedValueDefault
        , yearlyMaintenanceValue
        , passengersPerYearValue
        )
import Field exposing (SimpleField)
import BasicTiltak
import Tiltak exposing (Tiltak(..), StateCalculationMethod, bindTiltak, sendTo)


type alias SuperSimpleTiltak =
    { getter : TiltakStates.TiltakStates -> SuperSimpleCommonState
    , nytteMultiplikator : Float
    , stateMap : TiltakStates.StateMap SuperSimpleCommonState
    , title : String
    }


initialState : TiltakStates.SuperSimpleCommonState
initialState =
    { yearlyMaintenance = formattedValueDefault
    , passengersPerYear = formattedValueDefault
    , bompengeAndel = 0
    , preferredToGraph = ""
    }


fieldDefinitions : List (SimpleField TiltakStates.SuperSimpleCommonState)
fieldDefinitions =
    [ { name = "yearlyMaintenance"
      , title = "Årlige drifts- og vedlikeholdskostnader"
      , placeholder = "Årlige drifts- og vedlikeholdskostnader, kroner"
      , setter = Focus.set yearlyMaintenanceValue
      , accessor = Focus.get yearlyMaintenanceValue
      , stepSize = 5000
      }
    , { name = "passengersPerYear"
      , title = "Antall passasjerer per år"
      , placeholder = "Påstigende passasjerer per år"
      , setter = Focus.set passengersPerYearValue
      , accessor = Focus.get passengersPerYearValue
      , stepSize = 50
      }
    ]


createTiltak : SuperSimpleTiltak -> Tiltak
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
                        Just 0
            }
