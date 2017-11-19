module Field exposing (..)

import Focus exposing (Focus, (=>))


-- import TiltakStates exposing (TiltakStates)

import FormattedValue exposing (FormattedValue, Editable(..), state, value)
import TiltakStates exposing (TiltakStates)
import Models


type alias Field =
    Models.Field


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
            , value = Focus.get (simpleField.focus => value)
            }
    in
        fieldDefinitions
            |> List.map toRealField
