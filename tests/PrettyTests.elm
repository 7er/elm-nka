module PrettyTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import NumberFormat exposing (pretty, prettyTwoDecimals)


suite : Test
suite =
    describe "NumberFormat"
        [ describe "pretty"
            [ describe "thousand separates"
                [ test "1000 becomes '1 000'" (\_ -> pretty 1000 |> Expect.equal "1 000")
                , test "10000 becomes '10 000'" (\_ -> pretty 10000 |> Expect.equal "10 000")
                , test "100 becomes '100'" (\_ -> pretty 100 |> Expect.equal "100")
                , test "0 becomes '0'" (\_ -> pretty 0 |> Expect.equal "0")
                ]
            , describe "Rounds"
                [ test "3.14 becomes '3'" (\_ -> pretty 3.14 |> Expect.equal "3") ]
            , describe "Negative"
                [ test "-3.14 becomes '-3'" (\_ -> pretty -3.14 |> Expect.equal "-3")
                , test "-300 becomes '-300'" (\_ -> pretty -300 |> Expect.equal "-300")
                ]
            ]
        , describe
            "prettyTwoDecimals"
            [ describe "thousand separates"
                [ test "1000 becomes '1 000,00'" (\_ -> prettyTwoDecimals 1000 |> Expect.equal "1 000,00")
                , test "1 becomes '1,00'" (\_ -> prettyTwoDecimals 1 |> Expect.equal "1,00")
                , test "0 becomes '0,00'" (\_ -> prettyTwoDecimals 0 |> Expect.equal "0,00")
                ]
            , describe "gives 2 decimals"
                [ test "3.14 becomes '3,14'" (\_ -> prettyTwoDecimals 3.14 |> Expect.equal "3,14")
                , test "3.149 becomes '3,15'" (\_ -> prettyTwoDecimals 3.149 |> Expect.equal "3,15")
                , test "3.1 becomes '3,15'" (\_ -> prettyTwoDecimals 3.149 |> Expect.equal "3,15")
                ]
            , describe "Negative"
                [ test "-3.14 becomes '-3,14'" (\_ -> prettyTwoDecimals -3.14 |> Expect.equal "-3,14")
                , test "-3.149 becomes '-3,15'" (\_ -> prettyTwoDecimals -3.149 |> Expect.equal "-3,15")
                , test "-300 becomes '-300,00'" (\_ -> prettyTwoDecimals -300 |> Expect.equal "-300,00")
                ]
            ]
        ]
