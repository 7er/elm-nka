module Tiltak exposing (..)

import TiltakStates exposing (TiltakStates, StateMap)
import Charting


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
    , stringValueFromState : TiltakStates -> String
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
    -> ((specificState -> Maybe Float) -> (TiltakStates -> String))
    -> ((specificState -> Maybe Float) -> (TiltakStates -> Maybe Float))
    -> List (SimpleField specificState)
    -> List Field
transformToFields stateMap updateTiltakStateHelper stringValueHelper valueHelper fieldDefinitions =
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
            , stringValueFromState = stringValueHelper simpleField.accessor
            , value = valueHelper simpleField.accessor
            }
    in
        fieldDefinitions
            |> List.map toRealField


findVariableToGraph : Tiltak -> TiltakStates -> Maybe Field
findVariableToGraph this state =
    let
        filterFunc field =
            case field.value state of
                Just _ ->
                    False

                Nothing ->
                    True

        nothingFields =
            sendTo this .fields
                |> List.filter filterFunc
    in
        case nothingFields of
            [ head ] ->
                Just head

            _ ->
                Nothing


graphState : Tiltak -> TiltakStates -> GraphState
graphState this state =
    findVariableToGraph this state
        |> Maybe.map (always GraphOn)
        |> Maybe.withDefault GraphOff


graphDataForField this state field =
    let
        generateData x =
            let
                newState =
                    field.updateValue x state
            in
                sendTo this .nettoNytte newState |> Maybe.map (\y -> ( x, y ))

        sampleFunc x =
            let
                newState =
                    field.updateValue x state
            in
                case sendTo this .nettoNytte newState of
                    Just value ->
                        value

                    -- TODO: this is a bug
                    Nothing ->
                        42
    in
        Charting.samples field.stepSize sampleFunc
            |> List.map generateData
            |> List.filterMap identity


graphData : Tiltak -> TiltakStates -> List ( Float, Float )
graphData this state =
    let
        maybeField =
            findVariableToGraph this state
    in
        case maybeField of
            Nothing ->
                []

            Just field ->
                graphDataForField this state field
