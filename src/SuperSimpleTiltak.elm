module SuperSimpleTiltak exposing (..)

import Focus exposing (Focus, (=>))
import TiltakStates exposing (TiltakStates)
import SpecificStates
    exposing
        ( SimpleCommonState
        , SuperSimpleCommonState
        )
import FormattedValue
    exposing
        ( formattedValueDefault
        , passengersPerYear
        , yearlyMaintenance
        , value
        )
import Field exposing (SimpleField)
import BasicTiltak
import Tiltak exposing (Tiltak(..), StateCalculationMethod, bindTiltak, sendTo)


type alias SuperSimpleTiltak a =
    { nytteMultiplikator : Float
    , focus : Focus a SuperSimpleCommonState
    , title : String
    }


initialState : SuperSimpleCommonState
initialState =
    { yearlyMaintenance = formattedValueDefault
    , passengersPerYear = formattedValueDefault
    , bompengeAndel = 0
    , preferredToGraph = ""
    }


fieldDefinitions : Focus TiltakStates SuperSimpleCommonState -> List SimpleField
fieldDefinitions tiltakFocus =
    [ { name = "yearlyMaintenance"
      , title = "Årlige drifts- og vedlikeholdskostnader"
      , placeholder = "Årlige drifts- og vedlikeholdskostnader, kroner"
      , focus = (tiltakFocus => yearlyMaintenance)
      , stepSize = 5000
      }
    , { name = "passengersPerYear"
      , title = "Antall passasjerer per år"
      , placeholder = "Påstigende passasjerer per år"
      , focus = (tiltakFocus => passengersPerYear)
      , stepSize = 50
      }
    ]


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
