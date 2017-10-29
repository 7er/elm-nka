module Tiltak.Bussrenhold exposing (..)

import Tiltak exposing (Tiltak(..), StateCalculationMethod, sendTo)
import Field exposing (Field, SimpleField)
import TiltakStates exposing (TiltakStates, BussrenholdState)
import BasicTiltak
import GeneralForutsetninger exposing (verdisettinger)


yearlyPassasjerNytte : StateCalculationMethod
yearlyPassasjerNytte this ({ bussrenhold } as state) =
    bussrenhold.passengersPerYear
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
                            bussrenhold.dailyCostPerBus
                            bussrenhold.numberOfBusesAffected
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
    { dailyCostPerBus = Nothing
    , numberOfBusesAffected = Nothing
    , bompengeAndel = 0
    , passengersPerYear = Nothing
    , preferredToGraph = ""
    }


fieldDefinitions : List (SimpleField BussrenholdState)
fieldDefinitions =
    [ { name = "dailyCostPerBus"
      , title = "Kostnad per buss per dag"
      , placeholder = "kroner per buss per dag"
      , setter =
            (\value state ->
                { state
                    | dailyCostPerBus = value
                }
            )
      , accessor = .dailyCostPerBus
      , stepSize = 100
      }
    , { name = "numberOfBusesAffected"
      , title = "Antall busser som tiltaker gjelder"
      , placeholder = "Antallet i bussparken"
      , setter =
            (\value state ->
                { state
                    | numberOfBusesAffected = value
                }
            )
      , accessor = .numberOfBusesAffected
      , stepSize = 5000
      }
    , { name = "passengersPerYear"
      , title = "Antall passasjerer per år på busser som omfattes av tiltaket"
      , placeholder = "Årlige passasjerer ombord på busser med oppgradert renhold"
      , setter =
            (\value state ->
                { state
                    | passengersPerYear = value
                }
            )
      , accessor = .passengersPerYear
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
