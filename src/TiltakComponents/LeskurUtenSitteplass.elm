module TiltakComponents.LeskurUtenSitteplass exposing (..)

import Models exposing (Model, TiltakStates)
import Html exposing (Html, text)
import Msgs exposing (TiltakObject)


toggleVisible : TiltakStates -> TiltakStates
toggleVisible ({ leskurUtenSitteplassTiltakState } as tiltakStates) =
    { tiltakStates
        | leskurUtenSitteplassTiltakState = { leskurUtenSitteplassTiltakState | visible = not leskurUtenSitteplassTiltakState.visible }
    }


tiltakObject : TiltakObject
tiltakObject =
    { name = "Leskur u sitteplass"
    , page = \model -> [ text "Leskur side" ]
    , toggleVisible = toggleVisible
    , isVisible = \{ leskurUtenSitteplassTiltakState } -> leskurUtenSitteplassTiltakState.visible
    }
