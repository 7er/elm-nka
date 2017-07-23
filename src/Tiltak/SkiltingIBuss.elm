module Tiltak.SkiltingIBuss exposing (..)

import Tiltak exposing (TiltakNg(..))
import Tiltak.BasicTiltak as BasicTiltak


tiltak : TiltakNg
tiltak =
    let
        basicTiltakRecord =
            BasicTiltak.basicTiltakRecord
    in
        TiltakNg
            { basicTiltakRecord
                | title = \_ -> "Skilting i buss"
            }
