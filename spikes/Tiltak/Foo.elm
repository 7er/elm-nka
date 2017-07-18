module Tiltak.Foo exposing (..)

import TiltakStates exposing (TiltakStates, FooTiltakState)
import Types exposing (Tiltak)


initialState : FooTiltakState
initialState =
    { a = Just 3, b = Just 4 }


stateMap : (FooTiltakState -> FooTiltakState) -> TiltakStates -> TiltakStates
stateMap func tiltakStates =
    { tiltakStates | fooTiltak = func tiltakStates.fooTiltak }


calculation : TiltakStates -> Maybe Float
calculation { fooTiltak } =
    Maybe.map2 (*) fooTiltak.a fooTiltak.b


getFooState { fooTiltak } =
    fooTiltak


tiltak : Tiltak
tiltak =
    let
        updateTiltakStateHelper =
            TiltakStates.stateUpdateHelper stateMap

        thisStringValueHelper =
            TiltakStates.stringValueHelper getFooState
    in
        { calculation = calculation
        , title = "Foo tiltak"
        , fields =
            [ { name = "a"
              , title = "Alfa"
              , placeholder = "Dette er a variabelen"
              , updateTiltakState =
                    updateTiltakStateHelper
                        (\stringValue state ->
                            { state
                                | a = String.toFloat stringValue |> Result.toMaybe
                            }
                        )
              , stringValueFromState = thisStringValueHelper .a
              }
            , { name = "b"
              , title = "Beta"
              , placeholder = "Dette er b variabelen"
              , updateTiltakState =
                    updateTiltakStateHelper
                        (\stringValue state ->
                            { state
                                | b = String.toFloat stringValue |> Result.toMaybe
                            }
                        )
              , stringValueFromState = thisStringValueHelper .b
              }
            ]
        }
