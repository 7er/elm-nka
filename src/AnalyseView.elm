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
            [ h3 [] [ text "Konklusjon" ]
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
              , text "Verdien i dag av passasjerenes tids- og bekvemmelighetsgevinster, over hele analyseperioden"
              )
            , ( "Øvrige trafikanters nytte over " ++ (toString data.analysePeriode) ++ " år"
              , data.trafikantNytte |> NumberFormat.maybePretty
              , text "Verdien i dag av effektene på øvrige trafikanter, over hele analyseperioden"
              )
            , ( "Operatørnytte over " ++ (toString data.analysePeriode) ++ " år"
              , data.operatoerNytte |> NumberFormat.maybePretty
              , text "Nåverdien av operatøren nytte over hele analyseperioden"
              )
            , ( "Sum nytteelementer over " ++ (toString data.analysePeriode) ++ " år"
              , data.nytte |> NumberFormat.maybePretty
              , text "-"
              )
            , ( "Sum kostnader over " ++ (toString data.analysePeriode) ++ " år"
              , data.kostUtenSkyggepris |> NumberFormat.maybePretty
              , text "Verdien i dag av alle kostnader (drift/vedlikehold, investering og ev. gjeninvestering) som vil påløpe i analyseperioden"
              )
            , ( "Skyggepris offentlige midler"
              , data.skyggepris |> NumberFormat.maybePretty
              , text "Samfunnskostnaden (effektivitetstapet) som påløper når tiltaket er skattefinansiert"
              )
            , ( "Tiltakets nettonåverdi"
              , data.nettoNytte |> NumberFormat.maybePretty
              , text "Nytteelementer minus kostnadselementer. Tiltaket er lønnsomt hvis tallet er positivt."
              )
            , ( "Nettonytte per budsjettkrone (nyttekostnadsbrøk)"
              , data.nettoNyttePerBudsjettKrone |> NumberFormat.maybePrettyTwoDecimals
              , text "Uttrykk for hvor mye samfunnet får igjen for hver krone tiltaket koster. Tiltaket er lønnsomt hvis tallet er positivt"
              )
            ]

        gridRow ( title, value, popoverContent ) =
            Grid.row [ Row.attrs [ class "analyseColumn" ] ]
                [ Grid.col []
                    [ div [ class "nkaTooltip" ]
                        [ text title
                        , span [ class "nkaTooltipContent" ] [ popoverContent ]
                        ]
                    ]
                , Grid.col [ Col.attrs [ class "text-right" ] ]
                    [ text value ]
                ]

        dataRows =
            titleAndValueList |> List.map gridRow

        gridRows =
            dataRows ++ [ conclusionRow data ]
    in
        h3 [] [ text "Samfunnsøkonomisk analyse" ] :: gridRows
