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


type alias Field =
    { name : String
    , title : String
    , placeholder : String
    , storeFunc : SykkelparkeringUteFormState -> String -> SykkelparkeringUteFormState
    }


initialFormState : SykkelparkeringUteFormState
initialFormState =
    { tripsPerYear = Nothing
    , yearlyMaintenance = Nothing
    , installationCost = Just 300004
    , submitted = False
    }


findField : String -> Maybe Field
findField variableName =
    fields
        |> List.filter (\{ name } -> name == variableName)
        |> List.head


updateFormState : SykkelparkeringUteFormState -> VariableName -> String -> SykkelparkeringUteFormState
updateFormState formState variableName stringValue =
    case findField variableName of
        Just { storeFunc } ->
            storeFunc formState stringValue

        _ ->
            Debug.crash "TODO"


c3GraphId =
    "sykkelparkeringUteGraph"


loadGraph : Cmd msg
loadGraph =
    generateC3 c3GraphId


fields : List Field
fields =
    [ Field "tripsPerYear"
        "Antall sykkelreiser per år"
        "Sykkelreiser som bruker tiltaket"
      <|
        \formState -> \stringValue -> { formState | tripsPerYear = String.toInt stringValue |> Result.toMaybe }
    , Field "installationCost"
        "Installasjonskostnad"
        ""
      <|
        \formState -> \stringValue -> Debug.crash "TODO"
    , Field "yearlyMaintenance"
        "Årlige drifts- og vedlikeholdskostnader"
        "Kostnaden ved å installere tiltaket en gang, kroner"
      <|
        \formState -> \stringValue -> { formState | yearlyMaintenance = String.toFloat stringValue |> Result.toMaybe }
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
        (List.append
            (fields
                |> List.map
                    (\{ name, title, placeholder } ->
                        Form.group []
                            [ Form.label [ for name ] [ text title ]
                            , Input.number
                                [ Input.id name
                                , Input.placeholder placeholder
                                , Input.onInput (SykkelparkeringUteForm name)
                                ]
                            ]
                    )
            )
            [ Form.group []
                [ Form.label [ for "variableToGraph" ] [ text "Velg verdi som skal vises på X-aksen i grafen" ]
                , Select.select [ Select.id "variableToGraph" ]
                    (fields
                        |> List.map (\{ name, title } -> Select.item [ value name ] [ text title ])
                    )
                ]
            , Button.button [ Button.primary ] [ text "Submit" ]
            ]
        )
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
