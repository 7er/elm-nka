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
import TiltakStates exposing (TiltakStates)
import Msgs exposing (Msg(..))
import Tiltak exposing (Tiltak, sendTo)
import AnalyseView
import TiltakCharting exposing (GraphState(..))
import NumberFormat
import Field exposing (Field)


chart : TiltakStates -> Tiltak -> Html Msg
chart tiltakStates tiltak =
    let
        graphId =
            sendTo tiltak .graphId

        graphNodeContent =
            case (TiltakCharting.graphState tiltak tiltakStates) of
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


tiltakCard : TiltakStates -> Tiltak -> Accordion.Card Msg
tiltakCard tiltakStates tiltak =
    let
        analyse =
            AnalyseView.view <| Tiltak.analyse tiltak tiltakStates

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
                    [ Card.custom <| div [] ([ tiltakForm tiltak tiltakStates ] ++ analyse) ]
                , Accordion.block
                    [ Card.blockAttrs
                        [ style
                            [ ( "backgroundColor", "lightGrey" )
                            , ( "height", "400px" )
                            ]
                        ]
                    ]
                    [ Card.custom <| chart tiltakStates tiltak
                    ]
                ]
            }


fieldView :
    Tiltak
    -> TiltakStates
    -> Field
    -> Html Msg
fieldView tiltak tiltakStates ({ name, title, placeholder } as field) =
    let
        isEditable =
            field.isEditable tiltakStates

        fieldValueString =
            field.value tiltakStates
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


tiltakForm : Tiltak -> TiltakStates -> Html Msg
tiltakForm tiltak tiltakStates =
    let
        bompengeAndelView =
            Form.group []
                [ Form.label [ for "bompenger" ]
                    [ text "Bompengefinansiering av kostnadene" ]
                , Checkbox.custom
                    [ Checkbox.attrs [ id "bompenger" ]
                    , Checkbox.onCheck <| UpdateBompengeAndel tiltak
                    , Checkbox.checked <| Tiltak.bompengeAndelBool tiltak tiltakStates
                    ]
                    "Tiltaket er finansiert med bompenger"
                ]
    in
        Form.form []
            ((sendTo tiltak .fields |> List.map (fieldView tiltak tiltakStates))
                ++ [ bompengeAndelView ]
            )
