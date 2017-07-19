module TiltakComponents.LeskurUtenSitteplass exposing (..)

import Models exposing (Model, Tiltak)


tiltak : Tiltak
tiltak =
    { title = "Leskur u sitteplass"
    , brukerNytte = \{ leskurUtenSitteplassTiltakState } -> Nothing
    , kostUtenSkyggepris = \_ -> Nothing
    , fields = []
    }
