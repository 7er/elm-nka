module TiltakView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Accordion as Accordion
import Bootstrap.Card as Card
import Models exposing (..)
import Msgs exposing (Msg(..))
import NumberFormat
import TiltakStates exposing (TiltakStates)


tiltakCard : Model -> Tiltak -> Accordion.Card Msg
tiltakCard model tiltak =
    let
        analyse =
            samfunnsOkonomiskAnalyse tiltak model.tiltakStates
    in
        Accordion.card
            { id = (tiltak.title)
            , options = []
            , header = Accordion.header [] <| Accordion.toggle [] [ text tiltak.title ]
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
        Form.form [] (tiltak.fields |> List.map fieldView)


samfunnsOkonomiskAnalyse : Tiltak -> TiltakStates -> List (Html Msg)
samfunnsOkonomiskAnalyse tiltak tiltakStates =
    [ h2 [] [ text "Samfunnsøkonomisk analyse" ]
    , Grid.row []
        [ Grid.col [] [ text "Brukernes nytte over 40 år" ]
        , Grid.col [ Col.attrs [ class "text-right" ] ]
            [ text (NumberFormat.maybePretty <| tiltak.brukerNytte tiltakStates) ]
        ]
    , Grid.row []
        [ Grid.col [] [ text "Sum kostnader over 40 år" ]
        , Grid.col [ Col.attrs [ class "text-right" ] ]
            [ text (NumberFormat.maybePretty <| tiltak.kostUtenSkyggepris tiltakStates) ]
        ]
    ]
