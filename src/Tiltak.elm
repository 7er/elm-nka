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


type TiltakNg
    = TiltakNg TiltakRecord


type alias StateCalculationMethod =
    TiltakNg -> TiltakStates -> Maybe Float


type alias TiltakRecord =
    { title : TiltakNg -> String
    , fields : TiltakNg -> List Field
    , passasjerNytte : StateCalculationMethod
    , trafikantNytte : StateCalculationMethod
    , operatoerNytte : StateCalculationMethod
    , nytte : StateCalculationMethod
    , skyggePris : StateCalculationMethod
    , kostUtenSkyggepris : StateCalculationMethod
    , nettoNytte : StateCalculationMethod
    , yearlyPassasjerNytte : StateCalculationMethod
    , driftOgVedlihKost : StateCalculationMethod
    , investeringsKostInklRestverdi : StateCalculationMethod
    }


type alias TiltakAccessor a =
    TiltakRecord -> TiltakNg -> a


sendTo : TiltakNg -> TiltakAccessor a -> a
sendTo ((TiltakNg object) as this) recordAccessor =
    recordAccessor object this


bindTiltak : TiltakNg -> a -> (TiltakAccessor (a -> b) -> b)
bindTiltak tiltak tiltakStates =
    \accessor -> sendTo tiltak accessor tiltakStates


type alias FieldValue =
    String


updateTiltakStateFromField : Field -> FieldValue -> TiltakStates -> TiltakStates
updateTiltakStateFromField field stringValue tiltakStates =
    field.updateTiltakState stringValue tiltakStates


analyse : TiltakNg -> TiltakStates -> AnalyseData
analyse tiltak tiltakStates =
    let
        f =
            bindTiltak tiltak tiltakStates
    in
        { passasjerNytte = f .passasjerNytte
        , analysePeriode = 40
        , kostUtenSkyggepris = f .kostUtenSkyggepris
        , isProfitable = Just True
        , trafikantNytte = f .trafikantNytte
        , operatoerNytte = f .operatoerNytte
        , nytte = f .nytte
        , skyggePris = f .skyggePris
        , nettoNytte = f .nettoNytte
        }
