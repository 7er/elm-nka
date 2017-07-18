module Types exposing (..)

import TiltakStates exposing (TiltakStates)


type alias Field =
    { name : String
    , title : String
    , placeholder : String
    , updateTiltakState : String -> TiltakStates -> TiltakStates
    , stringValueFromState : TiltakStates -> String
    }


type alias Tiltak =
    { calculation : TiltakStates -> Maybe Float
    , title : String
    , fields : List Field
    }
