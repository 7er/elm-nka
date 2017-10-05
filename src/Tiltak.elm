module Tiltak exposing (..)

import TiltakStates exposing (TiltakStates, StateMap)


type GraphState
    = GraphOff
    | GraphOn


type alias Field =
    { name : String
    , title : String
    , placeholder : String
    , updateTiltakState : String -> TiltakStates -> TiltakStates
    , updateValue : Float -> TiltakStates -> TiltakStates
    , stepSize : Float
    , value : TiltakStates -> Maybe Float
    }


type alias SimpleField stateType =
    { name : String
    , title : String
    , placeholder : String
    , setter :
        Maybe Float
        -> stateType
        -> stateType
    , accessor : stateType -> Maybe Float
    , stepSize : Float
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



{-
    Some invariants

   passasjerNytte + trafikantNytte + operatoerNytte == nytte

   nytte == (  yearlyPassasjerNytte
             + yearlyTrafikantNytte
             + yearlyOperatoerNytte) * afaktorVekst

   nettoNytte = nytte + kost -- kost is negative

-}


type alias TiltakRecord =
    { title : Tiltak -> String
    , fields : Tiltak -> List Field
    , passasjerNytte : StateCalculationMethod
    , trafikantNytte : StateCalculationMethod
    , operatoerNytte : StateCalculationMethod
    , nytte : StateCalculationMethod
    , skyggepris : StateCalculationMethod
    , skyggeprisHelper : Tiltak -> TiltakStates -> Float -> Maybe Float
    , kostUtenSkyggepris : StateCalculationMethod
    , nettoNytte : StateCalculationMethod
    , yearlyPassasjerNytte : StateCalculationMethod
    , yearlyTrafikantNytte : StateCalculationMethod
    , yearlyOperatoerNytte : StateCalculationMethod
    , driftOgVedlihKost : StateCalculationMethod
    , investeringsKostInklRestverdi : StateCalculationMethod
    , graphState : Tiltak -> TiltakStates -> GraphState
    , graphId : Tiltak -> String
    , domId : Tiltak -> String
    , graphData : Tiltak -> TiltakStates -> List ( Float, Float )
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


transformToFields :
    StateMap specificState
    ->
        ((String -> specificState -> specificState)
         -> (String -> TiltakStates -> TiltakStates)
        )
    -> ((specificState -> Maybe Float) -> (TiltakStates -> Maybe Float))
    -> List (SimpleField specificState)
    -> List Field
transformToFields stateMap updateTiltakStateHelper valueHelper fieldDefinitions =
    let
        toRealField simpleField =
            { name = simpleField.name
            , title = simpleField.title
            , placeholder = simpleField.placeholder
            , stepSize = simpleField.stepSize
            , updateTiltakState =
                updateTiltakStateHelper
                    (\stringValue state ->
                        let
                            pipeline =
                                String.toFloat stringValue
                                    |> Result.toMaybe
                                    |> simpleField.setter
                        in
                            pipeline state
                    )
            , updateValue =
                \value tiltakStates ->
                    tiltakStates
                        |> stateMap
                            (\specificState ->
                                simpleField.setter (Just value) specificState
                            )
            , value = valueHelper simpleField.accessor
            }
    in
        fieldDefinitions
            |> List.map toRealField
