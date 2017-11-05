module Tiltak.Bussrenhold exposing (..)

import Focus exposing (..)
import Tiltak exposing (Tiltak(..), StateCalculationMethod, sendTo)
import Field exposing (Field, SimpleField)
import TiltakStates
    exposing
        ( TiltakStates
        , BussrenholdState
        , value
        , passengersPerYear
        , formattedValueDefault
        )
import BasicTiltak
import GeneralForutsetninger exposing (verdisettinger)


yearlyPassasjerNytte : StateCalculationMethod
yearlyPassasjerNytte this ({ bussrenhold } as state) =
    bussrenhold
        |> Focus.get (passengersPerYear => value)
        |> Maybe.map
            (\passengersPerYear ->
                0.5 * passengersPerYear * verdisettinger.bussrenhold
            )


tiltak : Tiltak
tiltak =
    let
        basicTiltakRecord =
            BasicTiltak.basicTiltakRecord
    in
        Tiltak
            { basicTiltakRecord
                | title = \_ -> "Ekstra renhold om bord på bussene"
                , fields = \_ -> fields
                , yearlyPassasjerNytte = yearlyPassasjerNytte
                , investeringsKostInklRestverdi =
                    \_ _ ->
                        Just 0
                , driftOgVedlihKost =
                    \_ { bussrenhold } ->
                        Maybe.map2
                            (\dailyCostPerBus numberOfBusesAffected ->
                                dailyCostPerBus * numberOfBusesAffected * 360 * GeneralForutsetninger.afaktor
                            )
                            bussrenhold.dailyCostPerBus.value
                            bussrenhold.numberOfBusesAffected.value
                            |> Maybe.map negate
                , skyggepris =
                    \this ({ bussrenhold } as state) ->
                        sendTo
                            this
                            .skyggeprisHelper
                            state
                            bussrenhold.bompengeAndel
            }


initialState : BussrenholdState
initialState =
    { dailyCostPerBus = formattedValueDefault
    , numberOfBusesAffected = formattedValueDefault
    , bompengeAndel = 0
    , passengersPerYear = formattedValueDefault
    , preferredToGraph = ""
    }


fieldDefinitions : List (SimpleField BussrenholdState)
fieldDefinitions =
    let
        dailyCostPerBus =
            Focus.create
                .dailyCostPerBus
                (\f state ->
                    { state
                        | dailyCostPerBus = f state.dailyCostPerBus
                    }
                )

        numberOfBusesAffected =
            Focus.create
                .numberOfBusesAffected
                (\f state ->
                    { state
                        | numberOfBusesAffected = f state.numberOfBusesAffected
                    }
                )
    in
        [ { name = "dailyCostPerBus"
          , title = "Kostnad per buss per dag"
          , placeholder = "kroner per buss per dag"
          , setter = Focus.set (dailyCostPerBus => value)
          , accessor = Focus.get (dailyCostPerBus => value)
          , stepSize = 100
          }
        , { name = "numberOfBusesAffected"
          , title = "Antall busser som tiltaker gjelder"
          , placeholder = "Antallet i bussparken"
          , setter = Focus.set (numberOfBusesAffected => value)
          , accessor = Focus.get (numberOfBusesAffected => value)
          , stepSize = 5000
          }
        , { name = "passengersPerYear"
          , title = "Antall passasjerer per år på busser som omfattes av tiltaket"
          , placeholder = "Årlige passasjerer ombord på busser med oppgradert renhold"
          , setter = Focus.set (passengersPerYear => value)
          , accessor = Focus.get (passengersPerYear => value)
          , stepSize = 50
          }
        ]


fields : List Field
fields =
    let
        stateMap updater tiltakStates =
            { tiltakStates
                | bussrenhold = updater tiltakStates.bussrenhold
            }

        updateTiltakStateHelper =
            TiltakStates.stateUpdateHelper stateMap

        thisValueHelper =
            TiltakStates.valueHelper .bussrenhold
    in
        fieldDefinitions
            |> Field.transformToFields
                stateMap
                updateTiltakStateHelper
                thisValueHelper
