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
import Tiltak exposing (Tiltak, sendTo, GraphState(..))
import AnalyseView


tiltakCard : Model -> Tiltak -> Accordion.Card Msg
tiltakCard model tiltak =
    let
        analyse =
            AnalyseView.view <| Tiltak.analyse tiltak model.tiltakStates

        title =
            sendTo tiltak .title

        graphId =
            sendTo tiltak .graphId

        graphNodeContent =
            case (sendTo tiltak .graphState model.tiltakStates) of
                GraphOn ->
                    []

                GraphOff ->
                    [ h3 [] [ text "Mangler data for å grafe" ] ]
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
                    [ Card.custom <| div [ id graphId ] graphNodeContent
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
                    , Input.value <| field.stringValueFromState model.tiltakStates
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
                                    , stringValueFromState = \_ -> "Flesk"
                                    , stepSize = 1
                                    , updateValue = \_ state -> state
                                    }
                            , Checkbox.checked False
                            ]
                            "What is the experiment"
                        ]
                   ]
            )
