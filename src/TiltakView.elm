module TiltakView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html
import Html.Events exposing (onBlur, onFocus)
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Input as Input
import Bootstrap.Accordion as Accordion
import Bootstrap.Card as Card
import Models exposing (Model)
import Msgs exposing (Msg(..))
import Tiltak exposing (Tiltak, sendTo)
import AnalyseView
import TiltakCharting exposing (GraphState(..))
import NumberFormat
import Field exposing (Field)


chart : Model -> Tiltak -> Html Msg
chart model tiltak =
    let
        graphId =
            sendTo tiltak .graphId

        graphNodeContent =
            case (TiltakCharting.graphState tiltak model.tiltakStates) of
                GraphOn ->
                    []

                GraphOff ->
                    [ h3 [] [ text "Mangler data for å grafe" ] ]
    in
        div []
            [ div [ id graphId ] graphNodeContent
            , div []
                [ div [] [ text """Grafen viser hvordan tiltakets nettonåverdi
                                 varierer med <forutsetnings navn her>""" ]
                , div []
                    [ text "Vis heller: "
                    , a [] [ text "<Annen forutsetning>" ]
                    , a [] [ text "<Tredje forutseting>" ]
                    ]
                ]
            ]


tiltakCard : Model -> Tiltak -> Accordion.Card Msg
tiltakCard model tiltak =
    let
        analyse =
            AnalyseView.view <| Tiltak.analyse tiltak model.tiltakStates

        title =
            sendTo tiltak .title
    in
        Accordion.card
            { id = sendTo tiltak .domId
            , options = []
            , header = Accordion.headerH4 [] <| Accordion.toggle [] [ text title ]
            , blocks =
                [ Accordion.block
                    []
                    [ Card.custom <| div [] ([ tiltakForm tiltak model ] ++ analyse) ]
                , Accordion.block
                    [ Card.blockAttrs
                        [ style
                            [ ( "backgroundColor", "lightGrey" )
                            , ( "height", "400px" )
                            ]
                        ]
                    ]
                    [ Card.custom <| chart model tiltak
                    ]
                ]
            }


fieldView :
    Tiltak
    -> Model
    -> Field
    -> Html Msg
fieldView tiltak model ({ name, title, placeholder } as field) =
    let
        isEditable =
            field.isEditable model.tiltakStates

        fieldValueString =
            field.value model.tiltakStates
                |> (case isEditable of
                        True ->
                            Maybe.map toString

                        False ->
                            Maybe.map NumberFormat.pretty
                   )
                |> Maybe.withDefault ""

        inputElement =
            (case isEditable of
                False ->
                    Input.text

                True ->
                    Input.number
            )
    in
        Form.group []
            [ Form.label [ for name ] [ text title ]
            , inputElement
                [ Input.id name
                , Input.placeholder placeholder
                , Input.attrs
                    [ onBlur (FieldBlur field)
                    , onFocus (FieldFocus field)
                    ]
                , Input.onInput <| UpdateField tiltak field
                , Input.value fieldValueString
                ]
            ]


tiltakForm : Tiltak -> Model -> Html Msg
tiltakForm tiltak model =
    let
        maybeBompengeField =
            Tiltak.getAttr tiltak .maybeBompengeAndelField

        bompengeFieldOrEmpty =
            case maybeBompengeField of
                Just bompengeField ->
                    [ Form.group []
                        [ Form.label [ for "bompenger" ] [ text "Er tiltaket dekket av bompenger" ]
                        , Checkbox.checkbox
                            [ Checkbox.attrs [ id "bompenger" ]
                            , Checkbox.onCheck (UpdateBompengeAndel tiltak)
                            , Checkbox.checked False
                            ]
                            "The checkbox for bompenger"
                        ]
                    ]

                Nothing ->
                    []
    in
        Form.form []
            ((sendTo tiltak .fields |> List.map (fieldView tiltak model))
                ++ bompengeFieldOrEmpty
            )
