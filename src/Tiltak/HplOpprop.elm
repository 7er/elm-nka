module Tiltak.HplOpprop exposing (..)

import Focus exposing (..)
import TiltakStates
    exposing
        ( HplOppropState
        , value
        , passengersPerYear
        , formattedValueDefault
        )
import Field exposing (SimpleField)
import BasicTiltak
import Tiltak exposing (Tiltak(..), StateCalculationMethod, bindTiltak, sendTo)
import GeneralForutsetninger exposing (verdisettinger)


type alias HplOpprop =
    { getter : TiltakStates.TiltakStates -> HplOppropState
    , nytteMultiplikator : Float
    , stateMap : TiltakStates.StateMap HplOppropState
    , title : String
    }


initialState =
    { passengersPerYear = formattedValueDefault
    , preferredToGraph = ""
    }


fieldDefinitions : List (SimpleField HplOppropState)
fieldDefinitions =
    [ { name = "passengersPerYear"
      , title = "Antall passasjerer per år"
      , placeholder = "Påstigende passasjerer per år"
      , setter = Focus.set (passengersPerYear => value)
      , accessor = Focus.get (passengersPerYear => value)
      , stepSize = 50
      }
    ]


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
            { stateMap = Focus.update specificState
            , getter = Focus.get specificState
            , nytteMultiplikator = verdisettinger.hplOpprop
            , title = "Opprop av neste holdeplass om bord"
            }
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
