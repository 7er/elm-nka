module PageSeparatSykkelveg exposing (..)

import Html exposing (Html)
import ModelAndMsg exposing (..)
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
import NumberFormat
import SeparatSykkelvegTiltak exposing (SeparatSykkelvegTiltakModel)
import Field exposing (Field, FormState, VariableName, FieldValue)
import TiltakPage


fields : List (Field SeparatSykkelvegTiltakModel)
fields =
    [ Field "lengthKm"
        "Sykkelveiens lengde"
        "Lengde"
      <|
        \formState stringValue -> { formState | lengthKm = String.toFloat stringValue |> Result.toMaybe }
    , Field "tripsPerYear"
        "Antall sykkelreiser per år"
        "Sykkelreiser som bruker tiltaket"
      <|
        \formState stringValue -> { formState | tripsPerYear = String.toInt stringValue |> Result.toMaybe }
    , Field "minutesSaved"
        "Minutter spart"
        "Blabla"
      <|
        \formState stringValue -> { formState | minutesSaved = String.toFloat stringValue |> Result.toMaybe }
    , Field "investmentCost"
        "Investeringskostander"
        "Investeringskostnaden"
      <|
        \formState stringValue -> { formState | investmentCost = String.toFloat stringValue |> Result.toMaybe }
    ]


initialFormState : FormState SeparatSykkelvegTiltakModel
initialFormState =
    { lengthKm = Nothing
    , tripsPerYear = Nothing
    , minutesSaved = Nothing
    , investmentCost = Nothing
    , submitted = False
    }


c3GraphId : String
c3GraphId =
    "sykkelparkeringUteGraph"


loadGraph : Cmd msg
loadGraph =
    generateC3 c3GraphId


fromForm : FormState SeparatSykkelvegTiltakModel -> SeparatSykkelvegTiltakModel
fromForm { lengthKm, tripsPerYear, minutesSaved, investmentCost } =
    { lengthKm = lengthKm, tripsPerYear = tripsPerYear, minutesSaved = minutesSaved, investmentCost = investmentCost }


modelComputation : FormState SeparatSykkelvegTiltakModel -> (SeparatSykkelvegTiltakModel -> Maybe Float) -> String
modelComputation form computationFunc =
    form
        |> fromForm
        |> computationFunc
        |> NumberFormat.maybePretty


updateFieldInModel : String -> FieldValue -> Model -> Model
updateFieldInModel variableName stringValue ({ separatSykkelvegFormState } as model) =
    { model
        | separatSykkelvegFormState = TiltakPage.updateFormState separatSykkelvegFormState variableName stringValue fields
    }


handleSubmit : Model -> ( Model, Cmd Msg )
handleSubmit ({ separatSykkelvegFormState } as model) =
    let
        newState =
            { separatSykkelvegFormState | submitted = True }
    in
        ( { model | separatSykkelvegFormState = newState }, loadGraph )


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
                (SeparatSykkelvegTiltak.brukerNytte
                    |> modelComputation model.separatSykkelvegFormState
                )
            ]
        ]
    , Grid.row []
        [ Grid.col [] [ text "Sum kostnader over 40 år" ]
        , Grid.col [ Col.attrs [ class "text-right" ] ]
            [ text
                (SeparatSykkelvegTiltak.kostUtenSkyggepris
                    |> modelComputation model.separatSykkelvegFormState
                )
            ]
        ]
    ]
