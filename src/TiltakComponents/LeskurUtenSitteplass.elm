module TiltakComponents.LeskurUtenSitteplass exposing (..)

import Models exposing (Model, TiltakComponentState(..), NameToComponentStates)
import Html exposing (Html, text)
import Msgs exposing (TiltakObject)
import Field exposing (createTiltakState, TiltakState)


toggleVisible : NameToComponentStates -> NameToComponentStates
toggleVisible tiltakComponentState =
    Models.toggleVisibleFor tiltakObject.name tiltakComponentState


tiltakObject : TiltakObject
tiltakObject =
    let
        this =
            { name = "Leskur u sitteplass"
            , page = \model -> [ text "Leskur side" ]
            , toggleVisible = toggleVisible
            , isVisible = \model -> True
            , initialState = LeskurUtenSitteplassState (createTiltakState {})
            }
    in
        { this | isVisible = (\model -> myTiltakState this model |> Maybe.map (\state -> state.visible) |> Maybe.withDefault False) }


myTiltakState : TiltakObject -> Model -> Maybe (TiltakState {})
myTiltakState this model =
    case Models.tiltakStateFor this.name model.tiltakComponentState of
        Just tiltakComponentState ->
            case tiltakComponentState of
                LeskurUtenSitteplassState state ->
                    Just state

                _ ->
                    Debug.crash "State error"

        Nothing ->
            Debug.crash "State error"
