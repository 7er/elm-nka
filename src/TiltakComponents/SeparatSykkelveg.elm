module TiltakComponents.SeparatSykkelveg exposing (..)

import Html exposing (Html)
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Html exposing (Html, text, div, h2)
import Html.Attributes exposing (for, value, id, class)
import Models exposing (..)
import Msgs exposing (Msg(..), TiltakObject)
import NumberFormat
import SeparatSykkelvegTiltak exposing (SeparatSykkelvegTiltakModel)
import Field exposing (..)
import TiltakPage


fields : List (Field SeparatSykkelvegTiltakModel)
fields =
    [ Field "lengthKm"
        "Sykkelveiens lengde"
        "Lengde"
      <|
        \specificState stringValue ->
            { specificState
                | lengthKm = String.toFloat stringValue |> Result.toMaybe
            }
    , Field "tripsPerYear"
        "Antall sykkelreiser per år"
        "Sykkelreiser som bruker tiltaket"
      <|
        \specificState stringValue ->
            { specificState
                | tripsPerYear = String.toInt stringValue |> Result.toMaybe
            }
    , Field "minutesSaved"
        "Minutter spart"
        "Blabla"
      <|
        \specificState stringValue ->
            { specificState
                | minutesSaved = String.toFloat stringValue |> Result.toMaybe
            }
    , Field "investmentCost"
        "Investeringskostander"
        "Investeringskostnaden"
      <|
        \specificState stringValue ->
            { specificState
                | investmentCost = String.toFloat stringValue |> Result.toMaybe
            }
    ]


initialTiltakState : TiltakState SeparatSykkelvegTiltakModel
initialTiltakState =
    createTiltakState
        { lengthKm = Nothing
        , tripsPerYear = Nothing
        , minutesSaved = Nothing
        , investmentCost = Nothing
        }


c3GraphId : String
c3GraphId =
    "sykkelparkeringUteGraph"


loadGraph : Cmd msg
loadGraph =
    generateC3 c3GraphId


modelComputation : TiltakState SeparatSykkelvegTiltakModel -> (SeparatSykkelvegTiltakModel -> Maybe Float) -> String
modelComputation form computationFunc =
    form
        |> unwrapState
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
