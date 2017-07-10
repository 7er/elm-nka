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
import SykkelparkeringUteTiltak exposing (SykkelparkeringUteTiltakModel)
import NumberFormat
import Field exposing (Field, FormState, VariableName)


type alias Title =
    String


fields : List (Field SykkelparkeringUteTiltakModel)
fields =
    [ Field "tripsPerYear"
        "Antall sykkelreiser per år"
        "Sykkelreiser som bruker tiltaket"
      <|
        \formState stringValue -> { formState | tripsPerYear = String.toInt stringValue |> Result.toMaybe }
    , Field "installationCost"
        "Installasjonskostnad"
        ""
      <|
        \formState stringValue -> { formState | installationCost = String.toFloat stringValue |> Result.toMaybe }
    , Field "yearlyMaintenance"
        "Årlige drifts- og vedlikeholdskostnader"
        "Kostnaden ved å installere tiltaket en gang, kroner"
      <|
        \formState stringValue -> { formState | yearlyMaintenance = String.toFloat stringValue |> Result.toMaybe }
    ]


initialFormState : FormState SykkelparkeringUteTiltakModel
initialFormState =
    { tripsPerYear = Nothing
    , yearlyMaintenance = Nothing
    , installationCost = Just 300004
    , submitted = False
    }


updateFormState : FormState SykkelparkeringUteTiltakModel -> VariableName -> String -> FormState SykkelparkeringUteTiltakModel
updateFormState formState variableName stringValue =
    case Field.findField variableName fields of
        Just { storeFunc } ->
            storeFunc formState stringValue

        _ ->
            Debug.crash "TODO"


c3GraphId : String
c3GraphId =
    "sykkelparkeringUteGraph"


loadGraph : Cmd msg
loadGraph =
    generateC3 c3GraphId


fromForm : FormState SykkelparkeringUteTiltakModel -> SykkelparkeringUteTiltakModel
fromForm { tripsPerYear, yearlyMaintenance, installationCost } =
    { tripsPerYear = tripsPerYear
    , yearlyMaintenance = yearlyMaintenance
    , installationCost = installationCost
    }


modelComputation : FormState SykkelparkeringUteTiltakModel -> (SykkelparkeringUteTiltakModel -> Maybe Float) -> String
modelComputation form computationFunc =
    form
        |> fromForm
        |> computationFunc
        |> NumberFormat.maybePretty


updateFieldInModel : String -> String -> Model -> Model
updateFieldInModel variableName stringValue model =
    { model
        | sykkelParkeringUteFormState = updateFormState model.sykkelParkeringUteFormState variableName stringValue
    }


handleSubmit : Model -> ( Model, Cmd Msg )
handleSubmit ({ sykkelParkeringUteFormState } as model) =
    let
        newFormState =
            { sykkelParkeringUteFormState | submitted = True }
    in
        ( { model | sykkelParkeringUteFormState = newFormState }, loadGraph )


page : Model -> List (Html Msg)
page model =
    [ Form.form [ onSubmit (FormSubmit handleSubmit) ]
        (List.append
            (fields
                |> List.map
                    (\{ name, title, placeholder } ->
                        Form.group []
                            [ Form.label [ for name ] [ text title ]
                            , Input.number
                                [ Input.id name
                                , Input.placeholder placeholder
                                , Input.onInput (FieldUpdate (\stringValue model -> updateFieldInModel name stringValue model))
                                ]
                            ]
                    )
            )
            [ Form.group []
                [ Form.label [ for "variableToGraph" ] [ text "Velg verdi som skal vises på X-aksen i grafen" ]
                , fields
                    |> List.map (\{ name, title } -> Select.item [ value name ] [ text title ])
                    |> Select.select [ Select.id "variableToGraph" ]
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
