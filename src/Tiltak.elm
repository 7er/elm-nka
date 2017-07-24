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


type alias AnalyseData =
    { passasjerNytte : Maybe Float
    , kostUtenSkyggepris : Maybe Float
    , analysePeriode : Float
    , isProfitable : Maybe Bool
    , trafikantNytte : Maybe Float
    , operatoerNytte : Maybe Float
    , nytte : Maybe Float
    , skyggepris : Maybe Float
    , nettoNytte : Maybe Float
    , nettoNyttePerBudsjettKrone : Maybe Float
    }


type Tiltak
    = Tiltak TiltakRecord


type alias StateCalculationMethod =
    Tiltak -> TiltakStates -> Maybe Float


type alias TiltakRecord =
    { title : Tiltak -> String
    , fields : Tiltak -> List Field
    , passasjerNytte : StateCalculationMethod
    , trafikantNytte : StateCalculationMethod
    , operatoerNytte : StateCalculationMethod
    , nytte : StateCalculationMethod
    , skyggepris : StateCalculationMethod
    , kostUtenSkyggepris : StateCalculationMethod
    , nettoNytte : StateCalculationMethod
    , yearlyPassasjerNytte : StateCalculationMethod
    , driftOgVedlihKost : StateCalculationMethod
    , investeringsKostInklRestverdi : StateCalculationMethod
    }


type alias TiltakAccessor a =
    TiltakRecord -> Tiltak -> a


sendTo : Tiltak -> TiltakAccessor a -> a
sendTo ((Tiltak object) as this) recordAccessor =
    recordAccessor object this


bindTiltak : Tiltak -> a -> (TiltakAccessor (a -> b) -> b)
bindTiltak tiltak tiltakStates =
    \accessor -> sendTo tiltak accessor tiltakStates


type alias FieldValue =
    String


updateTiltakStateFromField : Field -> FieldValue -> TiltakStates -> TiltakStates
updateTiltakStateFromField field stringValue tiltakStates =
    field.updateTiltakState stringValue tiltakStates


analyse : Tiltak -> TiltakStates -> AnalyseData
analyse tiltak tiltakStates =
    let
        f =
            bindTiltak tiltak tiltakStates
    in
        { passasjerNytte = f .passasjerNytte
        , analysePeriode = 40
        , kostUtenSkyggepris = f .kostUtenSkyggepris
        , isProfitable = f .nettoNytte |> Maybe.map (\value -> value > 0)
        , trafikantNytte = f .trafikantNytte
        , operatoerNytte = f .operatoerNytte
        , nytte = f .nytte
        , skyggepris = f .skyggepris
        , nettoNytte = f .nettoNytte
        , nettoNyttePerBudsjettKrone =
            Maybe.map2
                (\nettoNytte kostUtenSkyggepris ->
                    nettoNytte / (negate kostUtenSkyggepris)
                )
                (f .nettoNytte)
                (f .kostUtenSkyggepris)
        }
