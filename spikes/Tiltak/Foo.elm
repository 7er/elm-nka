module Tiltak.Foo exposing (..)

import TiltakStates exposing (TiltakStates, FooTiltakState)
import Types exposing (Tiltak)


initialState =
    { a = Just 3, b = Just 4 }


stateMap : (FooTiltakState -> FooTiltakState) -> TiltakStates -> TiltakStates
stateMap func tiltakStates =
    { tiltakStates | fooTiltak = func tiltakStates.fooTiltak }


calculation : TiltakStates -> Maybe Float
calculation { fooTiltak } =
    Maybe.map2 (*) fooTiltak.a fooTiltak.b


tiltak : Tiltak
tiltak =
    { calculation = calculation
    , title = "Foo tiltak"
    , fields =
        [ { name = "a"
          , title = "Alfa"
          , placeholder = "Dette er a variabelen"
          , updateTiltakState =
                \stringValue tiltakStates ->
                    tiltakStates
                        |> stateMap
                            (\state ->
                                { state
                                    | a = String.toFloat stringValue |> Result.toMaybe
                                }
                            )
          }
        , { name = "b"
          , title = "Beta"
          , placeholder = "Dette er b variabelen"
          , updateTiltakState =
                \stringValue tiltakStates ->
                    tiltakStates
                        |> stateMap
                            (\state ->
                                { state
                                    | b = String.toFloat stringValue |> Result.toMaybe
                                }
                            )
          }
        ]
    }
