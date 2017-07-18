module TiltakStates exposing (..)


type alias BarTiltakState =
    { x : Maybe Int, y : Maybe Int }


type alias FooTiltakState =
    { a : Maybe Float
    , b : Maybe Float
    }


type alias TiltakStates =
    { fooTiltak : FooTiltakState
    , barTiltak : BarTiltakState
    }
