module TiltakView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
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
                [ div [] [ text "Grafen viser hvordan kost-nytte nåverdier varierer med <forutsetnings navn her>" ]
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


tiltakForm : Tiltak -> Model -> Html Msg
tiltakForm tiltak model =
    let
        fieldView ({ name, title, placeholder } as field) =
            Form.group []
                [ Form.label [ for name ] [ text title ]
                , Input.number
                    [ Input.id name
                    , Input.placeholder placeholder
                    , Input.onInput <| UpdateField tiltak field
                    , Input.value <| Maybe.withDefault "" <| Maybe.map toString <| field.value model.tiltakStates
                    ]
                ]
    in
        Form.form []
            ((sendTo tiltak .fields |> List.map fieldView)
                ++ [ Form.group []
                        [ Form.label [ for "experiment" ] [ text "Eksperiment" ]
                        , Checkbox.checkbox
                            [ Checkbox.attrs [ id "experiment" ]
                            , Checkbox.onCheck <|
                                UpdateBooleanField
                                    { name = "eksperiment"
                                    , title = "Flesk"
                                    , placeholder = ""
                                    , updateTiltakState = \_ state -> state
                                    , stepSize = 1
                                    , updateValue = \_ state -> state
                                    , value = \_ -> Nothing
                                    }
                            , Checkbox.checked False
                            ]
                            "What is the experiment"
                        ]
                   ]
            )
