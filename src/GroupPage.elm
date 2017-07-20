module GroupPage exposing (..)

import Html exposing (..)
import Models exposing (..)
import Msgs exposing (Msg(..))
import TiltakAndGroupData
import TiltakView
import Bootstrap.Accordion as Accordion


gruppePageView : Model -> List Tiltak -> Html Msg
gruppePageView model tiltakene =
    let
        tiltakCard tiltak =
            TiltakView.tiltakCard model tiltak

        allCards =
            tiltakene |> List.map tiltakCard
    in
        Accordion.config AccordionMsg
            |> Accordion.withAnimation
            |> Accordion.cards allCards
            |> Accordion.view model.accordionState


page : Group -> Model -> List (Html Msg)
page tiltaksGruppeType model =
    [ TiltakAndGroupData.tiltakForGroup tiltaksGruppeType |> gruppePageView model ]
