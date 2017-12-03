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
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Button as Button
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

        fieldToGraphName =
            TiltakCharting.maybeFieldToGraph
                tiltak
                tiltakStates
                |> Maybe.map .title
                |> Maybe.withDefault "WAT!!!!"

        fieldButton field =
            ButtonGroup.button
                [ Button.onClick <|
                    UpdateFieldToGraph tiltak field
                , Button.secondary
                ]
                [ text field.title ]

        alternateFieldsToGraph =
            case TiltakCharting.possibleFieldsToGraph tiltak tiltakStates of
                [] ->
                    []

                _ as fields ->
                    [ text
                        "Vis heller: "
                    , fields
                        |> List.map fieldButton
                        |> ButtonGroup.buttonGroup []
                    ]

        variableToGraphView =
            case (TiltakCharting.graphState tiltak tiltakStates) of
                GraphOn ->
                    [ div []
                        [ text <| """Grafen viser hvordan tiltakets
nettonåverdi varierer med """ ++ fieldToGraphName
                        ]
                    , div []
                        alternateFieldsToGraph
                    ]

                GraphOff ->
                    []

        graphNodeContent =
            case (TiltakCharting.graphState tiltak tiltakStates) of
                GraphOn ->
                    []

                GraphOff ->
                    [ h3 [] [ text "Du må legge inn flere tall for å vise grafen" ] ]
    in
        div []
            [ div [ id graphId ] graphNodeContent
            , div [] variableToGraphView
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
                    [ Card.custom <|
                        div []
                            ([ tiltakForm tiltak tiltakStates ]
                                ++ analyse
                            )
                    ]
                , Accordion.block
                    [ Card.blockAttrs
                        [ class "chartBlock"
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
