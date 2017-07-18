module Tiltak.Bar exposing (..)

import Types exposing (Tiltak)
import TiltakStates exposing (TiltakStates, BarTiltakState)


initialState : BarTiltakState
initialState =
    { x = Just 1, y = Just 2 }


calculation : TiltakStates -> Maybe Float
calculation { barTiltak } =
    Maybe.map toFloat (Maybe.map2 (+) barTiltak.x barTiltak.y)


getBarState : TiltakStates -> BarTiltakState
getBarState { barTiltak } =
    barTiltak


tiltak : Tiltak
tiltak =
    let
        stateMap : TiltakStates.StateMap BarTiltakState
        stateMap func tiltakStates =
            { tiltakStates | barTiltak = getBarState tiltakStates |> func }

        updateTiltakStateHelper : TiltakStates.Setter BarTiltakState -> String -> TiltakStates -> TiltakStates
        updateTiltakStateHelper =
            TiltakStates.stateUpdateHelper stateMap

        thisStringValueHelper =
            TiltakStates.stringValueHelper getBarState
    in
        { calculation = calculation
        , title = "Bar tiltak"
        , fields =
            [ { name = "x"
              , title = "Khi"
              , placeholder = "Dette er x variabelen"
              , updateTiltakState =
                    updateTiltakStateHelper
                        (\stringValue state ->
                            { state
                                | x = String.toInt stringValue |> Result.toMaybe
                            }
                        )
              , stringValueFromState = thisStringValueHelper .x
              }
            , { name = "y"
              , title = "Ypsilon"
              , placeholder = "Dette er y variabelen"
              , updateTiltakState =
                    updateTiltakStateHelper
                        (\stringValue state ->
                            { state
                                | y = String.toInt stringValue |> Result.toMaybe
                            }
                        )
              , stringValueFromState = thisStringValueHelper .y
              }
            ]
        }
