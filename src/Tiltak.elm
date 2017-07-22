module Tiltak exposing (..)

import TiltakStates exposing (TiltakStates)


--import Models exposing (Tiltak, AnalyseData)


type alias Field =
    { name : String
    , title : String
    , placeholder : String
    , updateTiltakState : String -> TiltakStates -> TiltakStates
    , stringValueFromState : TiltakStates -> String
    }


type alias Tiltak =
    { brukerNytte : TiltakStates -> Maybe Float
    , kostUtenSkyggepris : TiltakStates -> Maybe Float
    , title : String
    , fields : List Field
    }


type alias AnalyseData =
    { passasjerNytte : Maybe Float
    , kostUtenSkyggepris : Maybe Float
    , analysePeriode : Float
    , isProfitable : Maybe Bool
    , trafikantNytte : Maybe Float
    , operatoerNytte : Maybe Float
    , nytte : Maybe Float
    , skyggePris : Maybe Float
    , nettoNytte : Maybe Float
    }



{-
   type Tiltak
       = Tiltak TiltakRecord


   type alias TiltakRecord =
       { passsasjerNytte : Tiltak -> TiltakStates -> Maybe Float
       , kostUtenSkyggepris : Tiltak -> TiltakStates -> Maybe Float
       , title : String
       , fields : List Field
       }


   sendTo ((Tiltak object) as this) field =
       field object this

-}


type alias FieldValue =
    String


updateTiltakStateFromField : Field -> FieldValue -> TiltakStates -> TiltakStates
updateTiltakStateFromField field stringValue tiltakStates =
    field.updateTiltakState stringValue tiltakStates


analyse : Tiltak -> TiltakStates -> AnalyseData
analyse tiltak tiltakStates =
    -- { passasjerNytte = sendTo tiltak .passasjerNytte tiltakStates }
    { passasjerNytte = tiltak.brukerNytte tiltakStates
    , analysePeriode = 40
    , kostUtenSkyggepris = tiltak.kostUtenSkyggepris tiltakStates
    , isProfitable = Just True
    , trafikantNytte = Just 1000
    , operatoerNytte = Just 301
    , nytte = Just 500 -- dette skal være en sum av ting og tang
    , skyggePris = Just 300
    , nettoNytte = Just 2000
    }
