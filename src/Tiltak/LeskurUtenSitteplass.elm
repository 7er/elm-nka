module Tiltak.LeskurUtenSitteplass exposing (..)

import Tiltak exposing (Tiltak(..))
import Tiltak.BasicTiltak as BasicTiltak


tiltak : Tiltak
tiltak =
    let
        basicTiltakRecord =
            BasicTiltak.basicTiltakRecord
    in
        Tiltak
            { basicTiltakRecord | title = \_ -> "Leskur u sitteplass" }
