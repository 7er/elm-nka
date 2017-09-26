module Charting exposing (..)

import Set


breakEvenPoint : (Float -> Float) -> Maybe Float
breakEvenPoint func =
    let
        x0 =
            0

        x1 =
            1

        a =
            (func x1 - func x0) / (x1 - x0)

        b =
            func 0
    in
        case a of
            0 ->
                Nothing

            _ ->
                -b / a |> Just


stepCount : Float -> Float -> Float
stepCount stepSize number =
    number / stepSize |> round |> toFloat


roundToStepSize : Float -> Float -> Float
roundToStepSize stepSize number =
    stepSize * stepCount stepSize number


samplesFromBreakEvenPoint : Float -> Float -> List Float
samplesFromBreakEvenPoint stepSize nullPunkt =
    let
        samplesOnEachSide =
            4

        minimum =
            0

        stepClosestToNullPunkt =
            roundToStepSize stepSize nullPunkt

        start =
            max
                (stepClosestToNullPunkt - (stepSize * samplesOnEachSide))
                0

        steps =
            List.range 0 (samplesOnEachSide * 2)
                |> List.map toFloat
                |> List.map (\index -> start + index * stepSize)
    in
        case nullPunkt >= minimum of
            True ->
                steps
                    |> (::) nullPunkt
                    |> Set.fromList
                    |> Set.toList
                    |> List.sort

            False ->
                steps


samples : Float -> (Float -> Float) -> List Float
samples stepSize generateDataFunc =
    case breakEvenPoint generateDataFunc of
        Just nullPunkt ->
            samplesFromBreakEvenPoint stepSize nullPunkt

        Nothing ->
            []
