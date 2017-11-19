module Tiltak.HplOpprop exposing (..)

import Focus exposing (..)
import SpecificStates exposing (HplOppropState)
import FormattedValue
    exposing
        ( value
        , passengersPerYear
        , formattedValueDefault
        )
import Field exposing (SimpleField)
import BasicTiltak
import Tiltak exposing (Tiltak(..))
import GeneralForutsetninger exposing (verdisettinger)


initialState : HplOppropState
initialState =
    { passengersPerYear = formattedValueDefault
    , preferredToGraph = ""
    , bompengeAndel = 0 -- dette er tull HplOpprop har ikke bompenge andel
    }


fieldDefinitions tiltakFocus =
    [ { name = "passengersPerYear"
      , title = "Antall passasjerer per år"
      , placeholder = "Påstigende passasjerer per år"
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
            BasicTiltak.basicTiltakRecord specificState

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
