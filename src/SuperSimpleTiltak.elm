module SuperSimpleTiltak exposing (..)

import Focus exposing (Focus, (=>))
import TiltakStates
    exposing
        ( SimpleCommonState
        , SuperSimpleCommonState
        , TiltakStates
        , formattedValueDefault
        , passengersPerYear
        , yearlyMaintenance
        , value
        )
import Field exposing (SimpleField)
import BasicTiltak
import Tiltak exposing (Tiltak(..), StateCalculationMethod, bindTiltak, sendTo)


type alias SuperSimpleTiltak =
    { nytteMultiplikator : Float
    , focus : Focus TiltakStates SuperSimpleCommonState
    , title : String
    }


initialState : TiltakStates.SuperSimpleCommonState
initialState =
    { yearlyMaintenance = formattedValueDefault
    , passengersPerYear = formattedValueDefault
    , bompengeAndel = 0
    , preferredToGraph = ""
    }


fieldDefinitions : Focus TiltakStates SuperSimpleCommonState -> List (SimpleField SuperSimpleCommonState)
fieldDefinitions tiltakFocus =
    [ { name = "yearlyMaintenance"
      , title = "Årlige drifts- og vedlikeholdskostnader"
      , placeholder = "Årlige drifts- og vedlikeholdskostnader, kroner"
      , setter = Focus.set (yearlyMaintenance => value)
      , accessor = Focus.get (yearlyMaintenance => value)
      , focus = (tiltakFocus => yearlyMaintenance)
      , stepSize = 5000
      }
    , { name = "passengersPerYear"
      , title = "Antall passasjerer per år"
      , placeholder = "Påstigende passasjerer per år"
      , setter = Focus.set (passengersPerYear => value)
      , accessor = Focus.get (passengersPerYear => value)
      , focus = (tiltakFocus => passengersPerYear)
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
                        Field.transformToFields
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
                        BasicTiltak.driftOgVedlihKost <| Focus.get simpleTiltak.focus state
                , investeringsKostInklRestverdi =
                    \_ state ->
                        Just 0
            }
