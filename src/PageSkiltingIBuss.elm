module PageSkiltingIBuss exposing (..)

import Models exposing (Model)
import Html exposing (Html, text)
import Msgs exposing (TiltakWidget)


toggleVisible : Model -> Model
toggleVisible ({ skiltingIBussFormState } as model) =
    { model
        | skiltingIBussFormState = { skiltingIBussFormState | visible = not skiltingIBussFormState.visible }
    }


tiltakWidget : TiltakWidget
tiltakWidget =
    { name = "Skilting i buss"
    , page = \model -> [ text "Skilting i buss side" ]
    , toggleVisible = toggleVisible
    , isVisible = \{ skiltingIBussFormState } -> skiltingIBussFormState.visible
    }
