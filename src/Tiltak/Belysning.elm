module Tiltak.Belysning exposing (..)

import GeneralForutsetninger exposing (verdisettinger)
import Tiltak exposing (Tiltak(..), StateCalculationMethod, bindTiltak, sendTo)
import Field exposing (Field, SimpleField)
import BasicTiltak
import SimpleTiltak


tiltak : Tiltak
tiltak =
    let
        stateMap func tiltakStates =
            { tiltakStates
                | belysning = func tiltakStates.belysning
            }

        getter =
            .belysning

        nytteMultiplikator =
            verdisettinger.belysning

        levetid =
            20

        title =
            "Belysning på holdeplass"

        basicTiltakRecord =
            BasicTiltak.basicTiltakRecord
    in
        Tiltak
            { basicTiltakRecord
                | title = \_ -> title
                , fields =
                    \_ ->
                        Field.compileFields stateMap
                            getter
                            SimpleTiltak.fieldDefinitions
                , skyggepris =
                    \this state ->
                        sendTo this .skyggeprisHelper state ((getter state).bompengeAndel)
                , yearlyPassasjerNytte =
                    \_ state ->
                        (getter state).passengersPerYear
                            |> Maybe.map ((*) nytteMultiplikator)
                , driftOgVedlihKost =
                    \_ state ->
                        BasicTiltak.driftOgVedlihKost (getter state)
                , investeringsKostInklRestverdi =
                    \_ state ->
                        BasicTiltak.investeringsKostInklRestverdi
                            (getter state)
                            levetid
            }
