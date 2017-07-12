module PageLeskurUtenSitteplass exposing (..)

import Models exposing (Model)
import Html exposing (Html, text)
import Msgs exposing (TiltakWidget)


toggleVisible : Model -> Model
toggleVisible ({ leskurUtenSitteplassFormState } as model) =
    { model
        | leskurUtenSitteplassFormState = { leskurUtenSitteplassFormState | visible = not leskurUtenSitteplassFormState.visible }
    }


tiltakWidget : TiltakWidget
tiltakWidget =
    { name = "Leskur u sitteplass"
    , page = \model -> [ text "Leskur side" ]
    , toggleVisible = toggleVisible
    , isVisible = \{ leskurUtenSitteplassFormState } -> leskurUtenSitteplassFormState.visible
    }
