module Tiltak.LeskurUtenSitteplass exposing (..)

import Tiltak exposing (TiltakNg(..))
import Tiltak.BasicTiltak as BasicTiltak


tiltak : TiltakNg
tiltak =
    let
        basicTiltakRecord =
            BasicTiltak.basicTiltakRecord
    in
        TiltakNg
            { basicTiltakRecord | title = \_ -> "Leskur u sitteplass" }
