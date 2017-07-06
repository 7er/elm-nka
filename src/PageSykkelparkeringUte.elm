module PageSykkelparkeringUte exposing (..)

import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Html exposing (Html, text, div, h2)
import Html.Attributes exposing (for, value, id, class)
import Html.Events exposing (onSubmit)
import ModelAndMsg exposing (..)
import SykkelparkeringUteTiltak
import NumberFormat


type alias Title =
    String


initialFormState : SykkelparkeringUteFormState
initialFormState =
    { tripsPerYear = Nothing
    , yearlyMaintenance = Just 10000
    , installationCost = Just 300004
    , submitted = False
    }


updateFormState : SykkelparkeringUteFormState -> VariableName -> String -> SykkelparkeringUteFormState
updateFormState formState variableName stringValue =
    case variableName of
        "tripsPerYear" ->
            -- maybeValue : Maybe Int
            let
                maybeValue =
                    case String.toInt stringValue of
                        Ok value ->
                            Just value

                        Err message ->
                            Debug.log message Nothing
            in
                { formState | tripsPerYear = maybeValue }

        _ ->
            Debug.crash "TODO"


c3GraphId =
    "sykkelparkeringUteGraph"


loadGraph : Cmd msg
loadGraph =
    generateC3 c3GraphId



--variableNameAndTitle : (List (VariableName, Title))


variableNameAndTitle =
    [ ( "tripsPerYear", "Antall sykkelreiser per år" )
    , ( "yearlyMaintenance", "Årlige drifts- og vedlikeholdskostnader" )
    , ( "installationCost", "Installasjonskostnad" )
    ]


brukerNytte : SykkelparkeringUteFormState -> String
brukerNytte { tripsPerYear, yearlyMaintenance, installationCost } =
    case ( tripsPerYear, yearlyMaintenance, installationCost ) of
        ( Just a, Just b, Just c ) ->
            NumberFormat.pretty
                (SykkelparkeringUteTiltak.brukerNytte
                    { tripsPerYear = a
                    , yearlyMaintenance = toFloat b
                    , installationCost = toFloat c
                    }
                )

        _ ->
            "ugyldige inputverdier"


page : Model -> List (Html Msg)
page model =
    [ Form.form [ onSubmit SykkelparkeringUteSubmit ]
        [ Form.group []
            [ Form.label [ for "tripsPerYear" ] [ text "Antall sykkelreiser per år" ]
            , Input.number
                [ Input.id "tripsPerYear"
                , Input.placeholder "Sykkelreiser som bruker tiltaket"
                , Input.onInput (SykkelparkeringUteForm "tripsPerYear")
                ]
            ]
        , Form.group []
            [ Form.label [ for "installationCost" ] [ text "Installasjonskostnad" ]
            , Input.number [ Input.id "installationCost" ]
            ]
        , Form.group []
            [ Form.label [ for "yearlyMaintenance" ] [ text "Årlige drifts- og vedlikeholdskostnader" ]
            , Input.number [ Input.id "yearlyMaintenance" ]
            ]
        , Form.group []
            [ Form.label [ for "variableToGraph" ] [ text "Velg verdi som skal vises på X-aksen i grafen" ]
            , Select.select [ Select.id "variableToGraph" ]
                (variableNameAndTitle
                    |> List.map (\( name, title ) -> Select.item [ value name ] [ text title ])
                )
            ]
        , Button.button [ Button.primary ] [ text "Submit" ]
        ]
    , div [ id c3GraphId ] [ text "Her skal grafen rendres" ]
    , h2 [] [ text "Samfunnsøkonomisk analyse" ]
    , Grid.row []
        [ Grid.col [] [ text "Brukernes nytte over 40 år" ]
        , Grid.col [ Col.attrs [ class "text-right" ] ] [ text (brukerNytte model.sykkelParkeringUteFormState) ]
        ]
    ]
