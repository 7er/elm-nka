module Tiltak.HplOpprop exposing (..)

import TiltakStates exposing (HplOppropState)
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
    { passengersPerYear = Nothing
    , preferredToGraph = ""
    }


fieldDefinitions : List (SimpleField HplOppropState)
fieldDefinitions =
    [ { name = "passengersPerYear"
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


tiltak : Tiltak
tiltak =
    let
        basicTiltakRecord =
            BasicTiltak.basicTiltakRecord

        simpleTiltak =
            { stateMap =
                \func tiltakStates ->
                    { tiltakStates
                        | hplOpprop = func tiltakStates.hplOpprop
                    }
            , getter = .hplOpprop
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
                        (simpleTiltak.getter state).passengersPerYear
                            |> Maybe.map ((*) simpleTiltak.nytteMultiplikator)
                , driftOgVedlihKost =
                    \_ state ->
                        Just 0
                , investeringsKostInklRestverdi =
                    \_ state ->
                        Just 0
            }
