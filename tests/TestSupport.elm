module TestSupport exposing (..)

import Expect exposing (Expectation)


closeTo : Float -> Int -> Float -> Expectation
closeTo expected precision actual =
    let
        epsilon =
            toFloat (10 ^ (negate precision)) / 2

        difference =
            abs (expected - actual)
    in
        if difference < epsilon then
            Expect.pass
        else
            (toString actual) ++ " is not near enough to " ++ (toString expected) ++ " using " ++ (toString precision) ++ " digits of precision" |> Expect.fail


checkMaybe : (a -> Expectation) -> Maybe a -> Expectation
checkMaybe expectation maybeValue =
    maybeValue
        |> Maybe.map expectation
        |> Maybe.withDefault (Expect.fail <| "Got nothing")
