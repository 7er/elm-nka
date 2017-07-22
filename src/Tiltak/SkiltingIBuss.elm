module Tiltak.SkiltingIBuss exposing (..)

import Tiltak exposing (Tiltak)


tiltak : Tiltak
tiltak =
    { title = "Skilting i buss"
    , brukerNytte = \_ -> Nothing
    , kostUtenSkyggepris = \_ -> Nothing
    , fields = []
    }
