module TiltakCharting exposing (..)

import Charting
import Tiltak exposing (Tiltak, GraphState(..), sendTo)
import Field exposing (Field)
import TiltakStates exposing (TiltakStates)


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


graphDataForField :
    Tiltak
    -> TiltakStates
    -> Field
    -> List ( Float, Float )
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

                    Nothing ->
                        Debug.crash "nettoNytte gave Nothing"
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
