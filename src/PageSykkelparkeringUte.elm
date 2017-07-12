module PageSykkelparkeringUte exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Html exposing (Html, text, div, h2)
import Html.Attributes exposing (for, value, id, class)
import Models exposing (..)
import SykkelparkeringUteTiltak exposing (SykkelparkeringUteTiltakModel)
import NumberFormat
import Field exposing (..)
import TiltakPage
import Msgs exposing (Msg, TiltakObject)


type alias Title =
    String


fields : List (Field SykkelparkeringUteTiltakModel)
fields =
    [ Field "tripsPerYear"
        "Antall sykkelreiser per år"
        "Sykkelreiser som bruker tiltaket"
      <|
        \({ specificState } as formState) stringValue ->
            { formState | specificState = { specificState | tripsPerYear = String.toInt stringValue |> Result.toMaybe } }
    , Field "installationCost"
        "Installasjonskostnad"
        ""
      <|
        \({ specificState } as formState) stringValue ->
            { formState | specificState = { specificState | installationCost = String.toFloat stringValue |> Result.toMaybe } }
    , Field "yearlyMaintenance"
        "Årlige drifts- og vedlikeholdskostnader"
        "Kostnaden ved å installere tiltaket en gang, kroner"
      <|
        \({ specificState } as formState) stringValue ->
            { formState | specificState = { specificState | yearlyMaintenance = String.toFloat stringValue |> Result.toMaybe } }
    ]


initialTiltakState : TiltakState SykkelparkeringUteTiltakModel
initialTiltakState =
    createTiltakState
        { tripsPerYear = Nothing
        , yearlyMaintenance = Nothing
        , installationCost = Just 300004
        }


c3GraphId : String
c3GraphId =
    "sykkelparkeringUteGraph"


loadGraph : Cmd msg
loadGraph =
    generateC3 c3GraphId


modelComputation : TiltakState SykkelparkeringUteTiltakModel -> (SykkelparkeringUteTiltakModel -> Maybe Float) -> String
modelComputation form computationFunc =
    form
        |> unwrapState
        |> computationFunc
        |> NumberFormat.maybePretty


updateFieldInModel : String -> String -> Model -> Model
updateFieldInModel variableName stringValue model =
    { model
        | sykkelParkeringUteTiltakState = TiltakPage.updateTiltakState model.sykkelParkeringUteTiltakState variableName stringValue fields
    }


handleSubmit : Model -> ( Model, Cmd Msg )
handleSubmit ({ sykkelParkeringUteTiltakState } as model) =
    let
        newTiltakState =
            { sykkelParkeringUteTiltakState | submitted = True }
    in
        ( { model | sykkelParkeringUteTiltakState = newTiltakState }, loadGraph )


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
                    |> modelComputation model.sykkelParkeringUteTiltakState
                )
            ]
        ]
    , Grid.row []
        [ Grid.col [] [ text "Sum kostnader over 40 år" ]
        , Grid.col [ Col.attrs [ class "text-right" ] ]
            [ text
                (SykkelparkeringUteTiltak.kostUtenSkyggepris
                    |> modelComputation model.sykkelParkeringUteTiltakState
                )
            ]
        ]
    ]


toggleVisible : Model -> Model
toggleVisible ({ sykkelParkeringUteTiltakState } as model) =
    { model
        | sykkelParkeringUteTiltakState = { sykkelParkeringUteTiltakState | visible = not sykkelParkeringUteTiltakState.visible }
    }


tiltakObject : TiltakObject
tiltakObject =
    { name = "Sikker sykkelparkering ute"
    , page = page
    , toggleVisible = toggleVisible
    , isVisible = \{ sykkelParkeringUteTiltakState } -> sykkelParkeringUteTiltakState.visible
    }
