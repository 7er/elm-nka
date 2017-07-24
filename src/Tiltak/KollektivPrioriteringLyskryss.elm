module KollektivPrioriteringLyskryss exposing (..)


tiltak =
    let
        basicTiltakRecord =
            BasicTiltak.basicTiltakRecord
    in
        TiltakNg
            { basicTiltakRecord
                | title = "Kollektiv prioritering lyskryss"
            }
