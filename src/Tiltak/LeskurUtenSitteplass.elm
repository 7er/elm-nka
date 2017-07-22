module Tiltak.LeskurUtenSitteplass exposing (..)

import Tiltak exposing (Tiltak)


tiltak : Tiltak
tiltak =
    { title = "Leskur u sitteplass"
    , brukerNytte = \{ leskurUtenSitteplassTiltakState } -> Nothing
    , kostUtenSkyggepris = \_ -> Nothing
    , fields = []
    }