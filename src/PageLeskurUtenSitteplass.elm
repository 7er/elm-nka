module PageLeskurUtenSitteplass exposing (..)

import Models exposing (Model)
import Html exposing (Html, text)
import Msgs exposing (TiltakObject)


toggleVisible : Model -> Model
toggleVisible ({ leskurUtenSitteplassTiltakState } as model) =
    { model
        | leskurUtenSitteplassTiltakState = { leskurUtenSitteplassTiltakState | visible = not leskurUtenSitteplassTiltakState.visible }
    }


tiltakObject : TiltakObject
tiltakObject =
    { name = "Leskur u sitteplass"
    , page = \model -> [ text "Leskur side" ]
    , toggleVisible = toggleVisible
    , isVisible = \{ leskurUtenSitteplassTiltakState } -> leskurUtenSitteplassTiltakState.visible
    }
