module TiltakTests exposing (..)

import Expect exposing (Expectation)


-- import Fuzz exposing (Fuzzer, list, int, string)

import Test exposing (..)
import SykkelparkeringUteTiltak


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


checkNytteNullPunktForNullModel model =
    SykkelparkeringUteTiltak.nettoNytte model |> closeTo 0 3



--checkNytteNullPunktForNullModel
{-
   var checkNytteNullPunktForNullModel = function(tiltak, inputModel) {
     var nullModel = angular.extend({}, inputModel);
     expect(tiltak.nettoNytte(nullModel)).toBeCloseTo(0, 3);
     var schemaProperties = tiltak.forutsetningSchema().properties;
     var pass = true;
     for (var i = 0; i < schemaProperties.variableToGraph.enum.length; i++) {
       var currentVariable = schemaProperties.variableToGraph.enum[i];
       nullModel.variableToGraph = currentVariable;
       expect(tiltak.nettoNytteNullpunkt(nullModel)).toBeCloseTo(nullModel[currentVariable], 3);
     }
   };

-}


suite : Test
suite =
    describe "Tiltak Models"
        [ describe "SykkelparkeringUteTiltak"
            [ sykkelParkeringUteTest "calculates the nytte" <|
                \model ->
                    SykkelparkeringUteTiltak.nytte model |> closeTo 543262.891 3
            , describe "kost"
                [ sykkelParkeringUteTest "calculates the kost" <|
                    \model ->
                        SykkelparkeringUteTiltak.kost model |> closeTo 543262.89 3
                , sykkelParkeringUteTest "calculates totalCostNowValue" <|
                    \model ->
                        SykkelparkeringUteTiltak.totalCostNowValue model |> closeTo 452719 0
                ]
            , describe "investmentKostInklRestverdiValueToday calculations"
                [ test "investmentFactor" <|
                    \_ ->
                        SykkelparkeringUteTiltak.investmentFactor |> closeTo 2.4402698 7
                , sykkelParkeringUteTest "investmentKostInklRestverdiValueToday" <|
                    \model ->
                        SykkelparkeringUteTiltak.investmentKostInklRestverdiValueToday model.installationCost |> closeTo 254791 0
                ]
            , sykkelParkeringUteTest "calculates the maintenanceCost" <|
                \model ->
                    SykkelparkeringUteTiltak.maintenanceCost model.yearlyMaintenance |> closeTo 197928 0
            , sykkelParkeringUteTest "calculates the nettoNytte" <|
                \model ->
                    SykkelparkeringUteTiltak.nettoNytte model |> closeTo 0 3
            , generateTestWithModel <|
                (\_ ->
                    let
                        model =
                            { tripsPerYear = 965
                            , yearlyMaintenance = 10000
                            , installationCost = 1.044111345e5
                            }

                        testGenerator variable =
                            test ("for " ++ (toString variable)) <|
                                \_ ->
                                    let
                                        variableValue =
                                            case variable of
                                                SykkelparkeringUteTiltak.TripsPerYear ->
                                                    model.tripsPerYear
                                    in
                                        SykkelparkeringUteTiltak.nettoNytteNullPunkt variable model |> closeTo
                    in
                        SykkelparkeringUteTiltak.schemaVariablesToGraph |> List.map testGenerator
                )

            {-
               it('calculates the correct nettoNytteNullpunkt', function() {
                 checkNytteNullPunktForNullModel(tiltak, model);
               });
            -}
            ]
        ]


sykkelParkeringUteTest :
    String
    -> (SykkelparkeringUteTiltak.SykkelParkeringUteTiltakModel -> Expectation)
    -> Test
sykkelParkeringUteTest description testCase =
    let
        model =
            { tripsPerYear = 965
            , yearlyMaintenance = 10000
            , installationCost = 1.044111345e5
            }
    in
        test description <|
            \_ ->
                testCase model


generateTestWithModel : (() -> List Test) -> Test
generateTestWithModel testLambda =
    let
        flesk =
            []
    in
        testLambda () |> Test.concat
