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
    , yearlyMaintenance = Nothing
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
                    String.toInt stringValue |> Result.toMaybe
            in
                { formState | tripsPerYear = maybeValue }

        "yearlyMaintenance" ->
            -- maybeValue : Maybe Int
            let
                maybeValue =
                    String.toFloat stringValue |> Result.toMaybe
            in
                { formState | yearlyMaintenance = maybeValue }

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
    , ( "installationCost", "Installasjonskostnad" )
    , ( "yearlyMaintenance", "Årlige drifts- og vedlikeholdskostnader" )
    ]


fromForm : SykkelparkeringUteFormState -> SykkelparkeringUteTiltak.SykkelParkeringUteTiltakModel
fromForm { tripsPerYear, yearlyMaintenance, installationCost } =
    { tripsPerYear = tripsPerYear
    , yearlyMaintenance = yearlyMaintenance
    , installationCost = installationCost
    }


modelComputation : SykkelparkeringUteFormState -> (SykkelparkeringUteTiltak.SykkelParkeringUteTiltakModel -> Maybe Float) -> String
modelComputation form computationFunc =
    form
        |> fromForm
        |> computationFunc
        |> NumberFormat.maybePretty


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
            , Input.number
                [ Input.id "yearlyMaintenance"
                , Input.placeholder "Kostnaden ved å installere tiltaket en gang, kroner"
                , Input.onInput (SykkelparkeringUteForm "yearlyMaintenance")
                ]
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
        , Grid.col [ Col.attrs [ class "text-right" ] ]
            [ text
                (SykkelparkeringUteTiltak.brukerNytte
                    |> modelComputation model.sykkelParkeringUteFormState
                )
            ]
        ]
    , Grid.row []
        [ Grid.col [] [ text "Sum kostnader over 40 år" ]
        , Grid.col [ Col.attrs [ class "text-right" ] ]
            [ text
                (SykkelparkeringUteTiltak.kostUtenSkyggepris
                    |> modelComputation model.sykkelParkeringUteFormState
                )
            ]
        ]
    ]
