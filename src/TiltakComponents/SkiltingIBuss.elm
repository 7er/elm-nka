module TiltakComponents.SkiltingIBuss exposing (..)

import Models exposing (Tiltak)


tiltak : Tiltak
tiltak =
    { title = "Skilting i buss"
    , brukerNytte = \_ -> Nothing
    , kostUtenSkyggepris = \_ -> Nothing
    , fields = []
    }
