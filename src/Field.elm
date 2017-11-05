module Field exposing (..)

import TiltakStates exposing (TiltakStates, StateMap)


type alias Field =
    { name : String
    , title : String
    , placeholder : String
    , updateTiltakState : String -> TiltakStates -> TiltakStates
    , updateValue : Float -> TiltakStates -> TiltakStates
    , stepSize : Float
    , value : TiltakStates -> Maybe Float
    , isEditable : TiltakStates -> Bool
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


type alias FieldValue =
    String


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


compileFields :
    StateMap specificState
    -> (TiltakStates -> specificState)
    -> List (SimpleField specificState)
    -> List Field
compileFields stateMap stateGetter fieldDefinitions =
    let
        updateTiltakStateHelper =
            TiltakStates.stateUpdateHelper stateMap

        thisValueHelper =
            TiltakStates.valueHelper stateGetter
    in
        fieldDefinitions
            |> transformToFields
                stateMap
                updateTiltakStateHelper
                thisValueHelper
