module TiltakTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (Test, describe, test)
import TestSupport exposing (..)
import Charting
import BasicTiltak


suite : Test
suite =
    Test.concat
        ([ describe "checkMaybe"
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
         , test "toDomId" <|
            \() ->
                "Alarmsystem / nødtelefon: på holdeplass"
                    |> BasicTiltak.toDomId
                    |> Expect.equal "Alarmsystem-nødtelefon-på-holdeplass"
         , describe "samples"
            [ test "linear function" <|
                \() ->
                    let
                        func x =
                            x * 2 - 1.0e6
                    in
                        Charting.samples 5.0e4 func
                            |> Expect.equal
                                [ 3.0e5
                                , 3.5e5
                                , 4.0e5
                                , 4.5e5
                                , 5.0e5
                                , 5.5e5
                                , 6.0e5
                                , 6.5e5
                                , 7.0e5
                                ]
            , test "flat function gives empty list" <|
                \() ->
                    let
                        func _ =
                            5
                    in
                        Charting.samples 36 func |> Expect.equal []
            ]
         , describe "samplesFromBreakEvenPoint"
            [ test "when nullpunkt hits a a sample" <|
                \() ->
                    Charting.samplesFromBreakEvenPoint 5.0e4 5.0e5
                        |> Expect.equal
                            [ 3.0e5
                            , 3.5e5
                            , 4.0e5
                            , 4.5e5
                            , 5.0e5
                            , 5.5e5
                            , 6.0e5
                            , 6.5e5
                            , 7.0e5
                            ]
            , test "calculate exact breakEvenPoint and rounded to stepsize" <|
                \() ->
                    Charting.samplesFromBreakEvenPoint 10 101
                        |> Expect.equal
                            [ 60
                            , 70
                            , 80
                            , 90
                            , 100
                            , 101
                            , 110
                            , 120
                            , 130
                            , 140
                            ]
            , test "negative break-even point is not included" <|
                \() ->
                    Charting.samplesFromBreakEvenPoint 10 -101
                        |> Expect.equal
                            [ 0
                            , 10
                            , 20
                            , 30
                            , 40
                            , 50
                            , 60
                            , 70
                            , 80
                            ]
            ]
         ]
        )
