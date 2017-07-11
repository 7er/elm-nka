module PageSykkelparkeringUte exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Html exposing (Html, text, div, h2)
import Html.Attributes exposing (for, value, id, class)
import ModelAndMsg exposing (..)
import SykkelparkeringUteTiltak exposing (SykkelparkeringUteTiltakModel)
import NumberFormat
import Field exposing (Field, FormState, VariableName, FieldValue)
import TiltakPage


type alias Title =
    String


fields : List (Field (SykkelparkeringUteTiltakModel a))
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


initialFormState : FormState (SykkelparkeringUteTiltakModel {})
initialFormState =
    { tripsPerYear = Nothing
    , yearlyMaintenance = Nothing
    , installationCost = Just 300004
    , submitted = False
    }


c3GraphId : String
c3GraphId =
    "sykkelparkeringUteGraph"


loadGraph : Cmd msg
loadGraph =
    generateC3 c3GraphId


fromForm : FormState (SykkelparkeringUteTiltakModel a) -> SykkelparkeringUteTiltakModel {}
fromForm { tripsPerYear, yearlyMaintenance, installationCost } =
    { tripsPerYear = tripsPerYear
    , yearlyMaintenance = yearlyMaintenance
    , installationCost = installationCost
    }


modelComputation : FormState (SykkelparkeringUteTiltakModel a) -> (SykkelparkeringUteTiltakModel {} -> Maybe Float) -> String
modelComputation form computationFunc =
    form
        |> fromForm
        |> computationFunc
        |> NumberFormat.maybePretty


updateFieldInModel : String -> String -> Model -> Model
updateFieldInModel variableName stringValue model =
    { model
        | sykkelParkeringUteFormState = TiltakPage.updateFormState model.sykkelParkeringUteFormState variableName stringValue fields
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
    TiltakPage.form handleSubmit updateFieldInModel fields model
        ++ [ div [ id c3GraphId ] [ text "Her skal grafen rendres" ] ]
        ++ (samfunnsOkonomiskAnalyse model)


samfunnsOkonomiskAnalyse : Model -> List (Html Msg)
samfunnsOkonomiskAnalyse model =
    [ h2 [] [ text "Samfunnsøkonomisk analyse" ]
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
