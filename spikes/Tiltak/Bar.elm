module Tiltak.Bar exposing (..)

import Types exposing (Tiltak)
import TiltakStates exposing (TiltakStates)


stateMap func tiltakStates =
    { tiltakStates | barTiltak = func tiltakStates.barTiltak }


calculation : TiltakStates -> Maybe Float
calculation { barTiltak } =
    Maybe.map toFloat (Maybe.map2 (+) barTiltak.x barTiltak.y)


tiltak : Tiltak
tiltak =
    let
        stateUpdateHelper mapper stringValue tiltakStates =
            tiltakStates
                |> stateMap (\state -> mapper stringValue state)
    in
        { calculation = calculation
        , title = "Bar tiltak"
        , fields =
            [ { name = "x"
              , title = "Khi"
              , placeholder = "Dette er x variabelen"
              , updateTiltakState =
                    stateUpdateHelper
                        (\stringValue state ->
                            { state
                                | x = String.toInt stringValue |> Result.toMaybe
                            }
                        )
              }
            , { name = "y"
              , title = "Ypsilon"
              , placeholder = "Dette er y variabelen"
              , updateTiltakState =
                    stateUpdateHelper
                        (\stringValue state ->
                            { state
                                | y = String.toInt stringValue |> Result.toMaybe
                            }
                        )
              }
            ]
        }
