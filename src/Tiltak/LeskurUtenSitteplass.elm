module Tiltak.LeskurUtenSitteplass exposing (..)

import Tiltak exposing (Tiltak)


tiltak : Tiltak
tiltak =
    { title = "Leskur u sitteplass"
    , brukerNytte = \{ leskurUtenSitteplass } -> Nothing
    , kostUtenSkyggepris = \_ -> Nothing
    , fields = []
    }
