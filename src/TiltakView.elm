module TiltakView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Accordion as Accordion
import Bootstrap.Card as Card
import Models exposing (Model)
import Msgs exposing (Msg(..))
import Tiltak exposing (Tiltak, sendTo)
import AnalyseView


toDomId : String -> String
toDomId string =
    string |> String.words |> String.join "-"


tiltakCard : Model -> Tiltak -> Accordion.Card Msg
tiltakCard model tiltak =
    let
        analyse =
            AnalyseView.view <| Tiltak.analyse tiltak model.tiltakStates

        title =
            sendTo tiltak .title
    in
        Accordion.card
            { id = (toDomId title)
            , options = []
            , header = Accordion.header [] <| Accordion.toggle [] [ text title ]
            , blocks =
                [ Accordion.block []
                    [ Card.custom <| div [] ([ form tiltak model ] ++ analyse) ]
                ]
            }


form : Tiltak -> Model -> Html Msg
form tiltak model =
    let
        fieldView ({ name, title, placeholder } as field) =
            Form.group []
                [ Form.label [ for name ] [ text title ]
                , Input.number
                    [ Input.id name
                    , Input.placeholder placeholder
                    , Input.onInput <| UpdateField field
                    , Input.value <| field.stringValueFromState model.tiltakStates
                    ]
                ]
    in
        Form.form [] (sendTo tiltak .fields |> List.map fieldView)
