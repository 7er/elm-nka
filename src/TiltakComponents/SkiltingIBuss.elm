module TiltakComponents.SkiltingIBuss exposing (..)

import Models exposing (Model)
import Html exposing (Html, text)
import Msgs exposing (TiltakObject)


toggleVisible : Model -> Model
toggleVisible ({ skiltingIBussTiltakState } as model) =
    { model
        | skiltingIBussTiltakState = { skiltingIBussTiltakState | visible = not skiltingIBussTiltakState.visible }
    }


tiltakObject : TiltakObject
tiltakObject =
    { name = "Skilting i buss"
    , page = \model -> [ text "Skilting i buss side" ]
    , toggleVisible = toggleVisible
    , isVisible = \{ skiltingIBussTiltakState } -> skiltingIBussTiltakState.visible
    }
