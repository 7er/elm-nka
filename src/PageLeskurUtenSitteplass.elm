module PageLeskurUtenSitteplass exposing (..)

import Models exposing (Model)
import Html exposing (Html, text)
import Msgs exposing (TiltakObject)


toggleVisible : Model -> Model
toggleVisible ({ leskurUtenSitteplassFormState } as model) =
    { model
        | leskurUtenSitteplassFormState = { leskurUtenSitteplassFormState | visible = not leskurUtenSitteplassFormState.visible }
    }


tiltakObject : TiltakObject
tiltakObject =
    { name = "Leskur u sitteplass"
    , page = \model -> [ text "Leskur side" ]
    , toggleVisible = toggleVisible
    , isVisible = \{ leskurUtenSitteplassFormState } -> leskurUtenSitteplassFormState.visible
    }
