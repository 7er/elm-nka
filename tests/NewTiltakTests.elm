module NewTiltakTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (Test, describe, test)
import Dict exposing (Dict)


-- import SykkelparkeringUteTiltak exposing (SykkelParkeringUteTiltakModel)

import Tiltak exposing (Tiltak, TiltakForutsetninger)


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
        [ describe "inheritance"
            [ test "baseCase" <|
                \() ->
                    let
                        sut =
                            Tiltak.base
                    in
                        ( sut.sum (), sut.first, sut.second ) |> Expect.equal ( 7, 4, 3 )
            , test "derived" <|
                \() ->
                    let
                        sut =
                            Tiltak.derived
                    in
                        ( sut.sum (), sut.first, sut.second ) |> Expect.equal ( 3, 1, 2 )
            ]
        , describe "checkMaybe"
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
        , describe "SykkelparkeringUteTiltak"
            [ sykkelParkeringUteTest "calculates the nytte" <|
                \tiltak model ->
                    tiltak.nytte model |> checkMaybe (closeTo 543262.891 3)
            , describe "kost"
                [ sykkelParkeringUteTest "calculates the kost" <|
                    \tiltak model ->
                        tiltak.kost model |> checkMaybe (closeTo 543262.89 3)
                , sykkelParkeringUteTest "calculates totalCostNowValue" <|
                    \tiltak model ->
                        tiltak.totalCostNowValue model |> checkMaybe (closeTo 452719 0)
                ]
            , describe "investmentKostInklRestverdiValueToday calculations"
                [ test "investmentFactor" <|
                    \_ ->
                        Tiltak.investmentFactor |> closeTo 2.4402698 7
                , sykkelParkeringUteTest "investmentKostInklRestverdiValueToday" <|
                    \tiltak model ->
                        Tiltak.investmentKostInklRestverdiValueToday (Dict.get "installationCost" model)
                            |> checkMaybe (closeTo 254791 0)
                ]
            , sykkelParkeringUteTest "calculates the maintenanceCost" <|
                \tiltak model ->
                    Tiltak.maintenanceCost (Dict.get "yearlyMaintenance" model) |> checkMaybe (closeTo 197928 0)
            , sykkelParkeringUteTest "calculates the nettoNytte" <|
                \tiltak model ->
                    tiltak.nettoNytte model |> checkMaybe (closeTo 0 3)

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
                                                   SykkelparkeringUteTiltak.TripsPerYear ->
                                                       model.tripsPerYear
                                       in
                                           SykkelparkeringUteTiltak.nettoNytteNullPunkt variable model |> closeTo
                       in
                           SykkelparkeringUteTiltak.schemaVariablesToGraph |> List.map testGenerator


               {-
                  it('calculates the correct nettoNytteNullpunkt', function() {
                    checkNytteNullPunktForNullModel(tiltak, model);
                  });
               -}
            -}
            ]
        ]


sykkelParkeringUteTest : String -> (Tiltak -> TiltakForutsetninger -> Expectation) -> Test
sykkelParkeringUteTest description testCase =
    let
        model =
            Dict.empty
                |> Dict.insert "tripsPerYear" 965
                |> Dict.insert "yearlyMaintenance" 10000
                |> Dict.insert "installationCost" 1.044111345e5
    in
        test description <|
            \_ ->
                testCase Tiltak.sykkelparkeringUte model


generateTestWithModel : (() -> List Test) -> Test
generateTestWithModel testLambda =
    let
        flesk =
            []
    in
        testLambda () |> Test.concat
