module Tiltak.HplOpprop exposing (..)

import Focus exposing (..)
import TiltakStates
    exposing
        ( HplOppropState
        , TiltakStates
        , value
        , passengersPerYear
        , formattedValueDefault
        )
import Field exposing (SimpleField)
import BasicTiltak
import Tiltak exposing (Tiltak(..), StateCalculationMethod, bindTiltak, sendTo)
import GeneralForutsetninger exposing (verdisettinger)


initialState : HplOppropState
initialState =
    { passengersPerYear = formattedValueDefault
    , preferredToGraph = ""
    }


fieldDefinitions : Focus TiltakStates HplOppropState -> List (SimpleField HplOppropState)
fieldDefinitions tiltakFocus =
    [ { name = "passengersPerYear"
      , title = "Antall passasjerer per år"
      , placeholder = "Påstigende passasjerer per år"
      , setter = Focus.set (passengersPerYear => value)
      , accessor = Focus.get (passengersPerYear => value)
      , focus = (tiltakFocus => passengersPerYear)
      , stepSize = 50
      }
    ]


specificState : Focus { b | hplOpprop : a } a
specificState =
    Focus.create
        .hplOpprop
        (\f tiltakStates ->
            { tiltakStates | hplOpprop = f tiltakStates.hplOpprop }
        )


tiltak : Tiltak
tiltak =
    let
        basicTiltakRecord =
            BasicTiltak.basicTiltakRecord

        simpleTiltak =
            { nytteMultiplikator = verdisettinger.hplOpprop
            , title = "Opprop av neste holdeplass om bord"
            , focus = specificState
            }
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
                        Just 0
                , yearlyPassasjerNytte =
                    \_ state ->
                        state
                            |> Focus.get (specificState => passengersPerYear => value)
                            |> Maybe.map ((*) simpleTiltak.nytteMultiplikator)
                , driftOgVedlihKost =
                    \_ state ->
                        Just 0
                , investeringsKostInklRestverdi =
                    \_ state ->
                        Just 0
            }
