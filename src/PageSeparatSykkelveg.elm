module PageSeparatSykkelveg exposing (..)

import Html exposing (Html)
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Html exposing (Html, text, div, h2)
import Html.Attributes exposing (for, value, id, class)
import Models exposing (..)
import Msgs exposing (Msg(..), TiltakWidget)
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
    , visible = False
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
page ({ separatSykkelvegFormState } as model) =
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


toggleVisible : Model -> Model
toggleVisible ({ separatSykkelvegFormState } as model) =
    { model
        | separatSykkelvegFormState = { separatSykkelvegFormState | visible = not separatSykkelvegFormState.visible }
    }


tiltakWidget : TiltakWidget
tiltakWidget =
    { name = "Separat sykkelveg"
    , page = page
    , toggleVisible = toggleVisible
    , isVisible = \{ separatSykkelvegFormState } -> separatSykkelvegFormState.visible
    }
