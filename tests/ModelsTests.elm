module ModelsTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (Test, describe, test)
import Tiltak
import TiltakComponents.LeskurUtenSitteplass as LeskurUtenSitteplass


suite : Test
suite =
    describe "tiltakStateFor"
        [ test "get the state for LeskurUtenSitteplass" <|
            \_ ->
                let
                    componentState =
                        Tiltak.initialTiltakComponentState

                    tiltak =
                        LeskurUtenSitteplass.tiltakObject
                in
                    4 |> Expect.equal tiltak.toggleVisible model
        ]
