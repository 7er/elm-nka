module Field exposing (..)

import Focus exposing (Focus, (=>))
import TiltakStates exposing (TiltakStates, FormattedValue, Editable(..), state, value)


type alias Field =
    { name : String
    , title : String
    , placeholder : String
    , updateTiltakState : String -> TiltakStates -> TiltakStates
    , updateValue : Float -> TiltakStates -> TiltakStates
    , stepSize : Float
    , value : TiltakStates -> Maybe Float
    , isEditable : TiltakStates -> Bool
    , beDisplayMode : TiltakStates -> TiltakStates
    , beEditMode : TiltakStates -> TiltakStates
    , focus : Focus TiltakStates (FormattedValue Float)
    }


type alias SimpleField =
    { name : String
    , title : String
    , placeholder : String
    , stepSize : Float

    -- focus for TiltakStates to fields value
    , focus : Focus TiltakStates (FormattedValue Float)
    }


type alias FieldValue =
    String


transformToFields : List SimpleField -> List Field
transformToFields fieldDefinitions =
    let
        toRealField simpleField =
            { name = simpleField.name
            , title = simpleField.title
            , placeholder = simpleField.placeholder
            , stepSize = simpleField.stepSize
            , focus = simpleField.focus
            , isEditable =
                \tiltakStates ->
                    case Focus.get (simpleField.focus => state) tiltakStates of
                        Edit ->
                            True

                        Display ->
                            False
            , beDisplayMode =
                \tiltakStates ->
                    Focus.set (simpleField.focus => state) Display tiltakStates
            , beEditMode =
                \tiltakStates ->
                    Focus.set (simpleField.focus => state) Edit tiltakStates
            , updateTiltakState =
                \stringValue tiltakStates ->
                    let
                        maybeValue =
                            stringValue |> String.toFloat |> Result.toMaybe
                    in
                        Focus.set (simpleField.focus => value) maybeValue tiltakStates
            , updateValue =
                \val tiltakStates ->
                    Focus.set (simpleField.focus => value) (Just val) tiltakStates
            , value = Focus.get (simpleField.focus => value)
            }
    in
        fieldDefinitions
            |> List.map toRealField
