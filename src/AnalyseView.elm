module AnalyseView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import NumberFormat
import Tiltak exposing (AnalyseData)
import Msgs exposing (Msg(..))


conclusionRow : AnalyseData -> Html Msg
conclusionRow data =
    let
        conclusionContent isProfitable =
            [ span [ class "isProfitableMark" ]
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
        div [ class "conclusion" ]
            [ h4 [] [ text "Konklusjon" ]
            , p []
                (data.isProfitable
                    |> Maybe.map conclusionContent
                    |> Maybe.withDefault [ text "Ufullstendige data" ]
                )
            ]


view : AnalyseData -> List (Html Msg)
view data =
    let
        titleAndValueList =
            [ ( "Passasjerenes nytte over " ++ (toString data.analysePeriode) ++ " år"
              , data.passasjerNytte |> NumberFormat.maybePretty
              )
            , ( "Øvrige trafikanters nytte over " ++ (toString data.analysePeriode) ++ " år"
              , data.trafikantNytte |> NumberFormat.maybePretty
              )
            , ( "Operatørnytte over " ++ (toString data.analysePeriode) ++ " år"
              , data.operatoerNytte |> NumberFormat.maybePretty
              )
            , ( "Sum nytteelementer over " ++ (toString data.analysePeriode) ++ " år"
              , data.nytte |> NumberFormat.maybePretty
              )
            , ( "Sum kostnader over " ++ (toString data.analysePeriode) ++ " år"
              , data.kostUtenSkyggepris |> NumberFormat.maybePretty
              )
            , ( "Skyggepris offentlige midler"
              , data.skyggepris |> NumberFormat.maybePretty
              )
            , ( "Tiltakets nettonåverdi"
              , data.nettoNytte |> NumberFormat.maybePretty
              )
            , ( "Nettonytte per budsjettkrone (nyttekostnadsbrøk)"
              , data.nettoNyttePerBudsjettKrone |> NumberFormat.maybePrettyTwoDecimals
              )
            ]

        gridRow ( title, value ) =
            Grid.row [ Row.attrs [ class "analyseColumn" ] ]
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
