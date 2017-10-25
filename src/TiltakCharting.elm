module TiltakCharting exposing (..)

import Charting
import Tiltak exposing (Tiltak, sendTo)
import Field exposing (Field)
import TiltakStates exposing (TiltakStates)


type GraphState
    = GraphOff
    | GraphOn


chartRecord :
    Tiltak
    -> TiltakStates
    -> { data : List ( Float, Float ), domId : String, variableTitle : String }
chartRecord tiltak tiltakStates =
    { domId = sendTo tiltak .graphId
    , data = graphData tiltak tiltakStates
    , variableTitle =
        maybeFieldToGraph tiltak tiltakStates
            |> Maybe.map .title
            |> Maybe.withDefault "WAT!!!!"
    }


maybeFieldToGraph : Tiltak -> TiltakStates -> Maybe Field
maybeFieldToGraph tiltak state =
    let
        filterFunc field =
            case field.value state of
                Just _ ->
                    False

                Nothing ->
                    True

        nothingFields =
            sendTo tiltak .fields
                |> List.filter filterFunc
    in
        case nothingFields of
            [ head ] ->
                Just head

            [] ->
                Nothing

            -- Tiltak.preferredField tiltak state
            _ ->
                Nothing


graphState : Tiltak -> TiltakStates -> GraphState
graphState tiltak state =
    maybeFieldToGraph tiltak state
        |> Maybe.map (always GraphOn)
        |> Maybe.withDefault GraphOff


graphDataForField :
    Tiltak
    -> TiltakStates
    -> Field
    -> List ( Float, Float )
graphDataForField tiltak state field =
    let
        generateData x =
            let
                newState =
                    field.updateValue x state
            in
                sendTo tiltak .nettoNytte newState |> Maybe.map (\y -> ( x, y ))

        sampleFunc x =
            let
                newState =
                    field.updateValue x state
            in
                case sendTo tiltak .nettoNytte newState of
                    Just value ->
                        value

                    Nothing ->
                        Debug.crash "nettoNytte gave Nothing"
    in
        Charting.samples field.stepSize sampleFunc
            |> List.map generateData
            |> List.filterMap identity


graphData : Tiltak -> TiltakStates -> List ( Float, Float )
graphData tiltak state =
    let
        maybeField =
            maybeFieldToGraph tiltak state
    in
        case maybeField of
            Nothing ->
                []

            Just field ->
                graphDataForField tiltak state field
