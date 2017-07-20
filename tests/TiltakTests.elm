module TiltakTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (Test, describe, test)
import Tiltak.SykkelparkeringUte as SykkelparkeringUte
import TiltakStates exposing (SykkelparkeringUteTiltakModel)


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
    case maybeValue of
        Just value ->
            expectation value

        Nothing ->
            Expect.fail "Got nothing"


checkNytteNullPunktForNullModel : SykkelparkeringUteTiltakModel -> Expectation
checkNytteNullPunktForNullModel model =
    SykkelparkeringUte.nettoNytte model |> checkMaybe (closeTo 0 3)



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
        [ describe "checkMaybe"
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
        , describe "SykkelparkeringUte"
            [ sykkelParkeringUteTest "calculates the nytte" <|
                \model ->
                    SykkelparkeringUte.nytte model |> checkMaybe (closeTo 543262.891 3)
            , describe "kost"
                [ sykkelParkeringUteTest "calculates the kost" <|
                    \model ->
                        SykkelparkeringUte.kost model |> checkMaybe (closeTo 543262.89 3)
                , sykkelParkeringUteTest "calculates totalCostNowValue" <|
                    \model ->
                        SykkelparkeringUte.totalCostNowValue model |> checkMaybe (closeTo 452719 0)
                ]
            , describe "investmentKostInklRestverdiValueToday calculations"
                [ test "investmentFactor" <|
                    \_ ->
                        SykkelparkeringUte.investmentFactor |> closeTo 2.4402698 7
                , sykkelParkeringUteTest "investmentKostInklRestverdiValueToday" <|
                    \model ->
                        SykkelparkeringUte.investmentKostInklRestverdiValueToday model.installationCost |> checkMaybe (closeTo 254791 0)
                ]
            , sykkelParkeringUteTest "calculates the maintenanceCost" <|
                \model ->
                    SykkelparkeringUte.maintenanceCost model.yearlyMaintenance |> checkMaybe (closeTo 197928 0)
            , sykkelParkeringUteTest "calculates the nettoNytte" <|
                \model ->
                    SykkelparkeringUte.nettoNytte model |> checkMaybe (closeTo 0 3)

            {- , generateTestWithModel <|
                   \_ ->
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
                                                   SykkelparkeringUte.TripsPerYear ->
                                                       model.tripsPerYear
                                       in
                                           SykkelparkeringUte.nettoNytteNullPunkt variable model |> closeTo
                       in
                           SykkelparkeringUte.schemaVariablesToGraph |> List.map testGenerator


               {-
                  it('calculates the correct nettoNytteNullpunkt', function() {
                    checkNytteNullPunktForNullModel(tiltak, model);
                  });
               -}
            -}
            ]
        ]


sykkelParkeringUteTest : String -> (SykkelparkeringUteTiltakModel -> Expectation) -> Test
sykkelParkeringUteTest description testCase =
    let
        model =
            { tripsPerYear = Just 965
            , yearlyMaintenance = Just 10000
            , installationCost = Just 1.044111345e5
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
