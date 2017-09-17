module TiltakTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (Test, describe, test)
import TestSupport exposing (..)


suite : Test
suite =
    describe "checkMaybe"
        [ test "it always fails for Nothing" <|
            \_ ->
                let
                    expectation =
                        Nothing |> checkMaybe (Expect.equal 80)
                in
                    expectation |> Expect.equal (Expect.fail "Got nothing")
        , test "it can check equality for Just" <|
            \_ ->
                Just 80 |> checkMaybe (Expect.equal 80)
        ]
