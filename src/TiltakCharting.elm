module TiltakCharting exposing (..)

import Focus exposing ((=>))
import Charting
import Tiltak exposing (Tiltak, sendTo)
import FormattedValue exposing (value)


type GraphState
    = GraphOff
    | GraphOn


chartRecord tiltak tiltakStates =
    { domId = sendTo tiltak .graphId
    , data = graphData tiltak tiltakStates
    , variableTitle =
        maybeFieldToGraph tiltak tiltakStates
            |> Maybe.map .title
            |> Maybe.withDefault "WAT!!!!"
    }


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
                sendTo tiltak .preferredField state

            _ ->
                Nothing


possibleFieldsToGraph tiltak state =
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

        maybeFieldToGraphName =
            maybeFieldToGraph tiltak state
                |> Maybe.map .name
    in
        case nothingFields of
            [] ->
                sendTo tiltak .fields
                    |> case maybeFieldToGraphName of
                        Just name ->
                            List.filter (\field -> field.name /= name)

                        Nothing ->
                            identity

            _ ->
                []


graphState tiltak state =
    maybeFieldToGraph tiltak state
        |> Maybe.map (always GraphOn)
        |> Maybe.withDefault GraphOff


graphDataForField tiltak state field =
    let
        stateFrom x =
            Focus.set (field.focus => value) (Just x) state

        generateData x =
            sendTo tiltak .nettoNytte (stateFrom x)
                |> Maybe.map (\y -> ( x, y ))

        sampleFunc x =
            case sendTo tiltak .nettoNytte (stateFrom x) of
                Just value ->
                    value

                Nothing ->
                    Debug.crash "nettoNytte gave Nothing"
    in
        Charting.samples field.stepSize sampleFunc
            |> List.map generateData
            |> List.filterMap identity


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
