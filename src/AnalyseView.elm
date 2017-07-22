module AnalyseView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import NumberFormat
import Tiltak exposing (Tiltak, AnalyseData)
import Msgs exposing (Msg(..))


conclusionRow : AnalyseData -> Html Msg
conclusionRow data =
    let
        conclusionContent isProfitable =
            [ span [ style [ ( "font-size", "1.4em" ) ] ]
                [ text
                    (String.fromChar
                        (case isProfitable of
                            True ->
                                '✓'

                            False ->
                                '⚠'
                        )
                    )
                ]
            , text
                ("Tiltaket er "
                    ++ (case isProfitable of
                            True ->
                                ""

                            False ->
                                "ikke"
                       )
                    ++ " samfunnsøkonomisk lønnsomt."
                )
            ]
    in
        Grid.row []
            [ Grid.col []
                [ h4 [] [ text "Konklusjon" ]
                , div [ class "conclusion" ]
                    (data.isProfitable |> Maybe.map conclusionContent |> Maybe.withDefault [ text "Ufullstendige data" ])
                ]
            ]


view : AnalyseData -> List (Html Msg)
view data =
    let
        titleAndValueList =
            [ ( "Passasjerenes nytte over " ++ (toString data.analysePeriode) ++ " år"
              , data.passasjerNytte |> NumberFormat.maybePretty
              )
            , ( "Trafikantenes nytte over " ++ (toString data.analysePeriode) ++ " år"
              , data.trafikantNytte |> NumberFormat.maybePretty
              )
            , ( "Operatatør nytte over " ++ (toString data.analysePeriode) ++ " år"
              , data.operatoerNytte |> NumberFormat.maybePretty
              )
            , ( "Sum nytteelementer over " ++ (toString data.analysePeriode) ++ " år"
              , data.nytte |> NumberFormat.maybePretty
              )
            , ( "Sum kostnader over " ++ (toString data.analysePeriode) ++ " år"
              , data.kostUtenSkyggepris |> NumberFormat.maybePretty
              )
            , ( "Skyggepris offentlige midler"
              , data.skyggePris |> NumberFormat.maybePretty
              )
            , ( "Tiltakets nettonåverdi"
              , data.nettoNytte |> NumberFormat.maybePretty
              )
            , ( "Nettonytte per budsjettkrone (Nyttekostnadsbrøk)"
              , Maybe.map2 (/) data.nettoNytte data.kostUtenSkyggepris |> NumberFormat.maybePrettyTwoDecimals
              )
            ]

        gridRow ( title, value ) =
            Grid.row []
                [ Grid.col [] [ text title ]
                , Grid.col [ Col.attrs [ class "text-right" ] ]
                    [ text value ]
                ]

        dataRows =
            titleAndValueList |> List.map gridRow

        gridRows =
            dataRows ++ [ conclusionRow data ]
    in
        h3 [] [ text "Samfunnsøkonomisk analyse" ] :: gridRows
