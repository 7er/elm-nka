module PageSeparatSykkelveg exposing (..)

import Html exposing (Html)
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Html exposing (Html, text, div, h2)
import Html.Attributes exposing (for, value, id, class)
import Models exposing (..)
import Msgs exposing (Msg(..), TiltakObject)
import NumberFormat
import SeparatSykkelvegTiltak exposing (SeparatSykkelvegTiltakModel)
import Field exposing (Field, TiltakState, VariableName, FieldValue)
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


initialTiltakState : TiltakState SeparatSykkelvegTiltakModel
initialTiltakState =
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


fromForm : TiltakState SeparatSykkelvegTiltakModel -> SeparatSykkelvegTiltakModel
fromForm { lengthKm, tripsPerYear, minutesSaved, investmentCost } =
    { lengthKm = lengthKm, tripsPerYear = tripsPerYear, minutesSaved = minutesSaved, investmentCost = investmentCost }


modelComputation : TiltakState SeparatSykkelvegTiltakModel -> (SeparatSykkelvegTiltakModel -> Maybe Float) -> String
modelComputation form computationFunc =
    form
        |> fromForm
        |> computationFunc
        |> NumberFormat.maybePretty


updateFieldInModel : String -> FieldValue -> Model -> Model
updateFieldInModel variableName stringValue ({ separatSykkelvegTiltakState } as model) =
    { model
        | separatSykkelvegTiltakState = TiltakPage.updateTiltakState separatSykkelvegTiltakState variableName stringValue fields
    }


handleSubmit : Model -> ( Model, Cmd Msg )
handleSubmit ({ separatSykkelvegTiltakState } as model) =
    let
        newState =
            { separatSykkelvegTiltakState | submitted = True }
    in
        ( { model | separatSykkelvegTiltakState = newState }, loadGraph )


page : Model -> List (Html Msg)
page ({ separatSykkelvegTiltakState } as model) =
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
                    |> modelComputation model.separatSykkelvegTiltakState
                )
            ]
        ]
    , Grid.row []
        [ Grid.col [] [ text "Sum kostnader over 40 år" ]
        , Grid.col [ Col.attrs [ class "text-right" ] ]
            [ text
                (SeparatSykkelvegTiltak.kostUtenSkyggepris
                    |> modelComputation model.separatSykkelvegTiltakState
                )
            ]
        ]
    ]


toggleVisible : Model -> Model
toggleVisible ({ separatSykkelvegTiltakState } as model) =
    { model
        | separatSykkelvegTiltakState = { separatSykkelvegTiltakState | visible = not separatSykkelvegTiltakState.visible }
    }


tiltakObject : TiltakObject
tiltakObject =
    { name = "Separat sykkelveg"
    , page = page
    , toggleVisible = toggleVisible
    , isVisible = \{ separatSykkelvegTiltakState } -> separatSykkelvegTiltakState.visible
    }
