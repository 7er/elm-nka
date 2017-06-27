module Example exposing (..)

import Expect exposing (Expectation)
-- import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)


suite : Test
suite =
    describe "String.reverse" -- Nest as many descriptions as you like.
        [ test "has no effect on a palindrome" <|
            \_ ->
                let
                    palindrome =
                            "hannah"
                in
                    Expect.equal palindrome (String.reverse palindrome)
        ]
