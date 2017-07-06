module PrettyTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import NumberFormat exposing (pretty)


suite : Test
suite =
    describe "pretty"
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
