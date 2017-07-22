module GroupPage exposing (..)

import Html exposing (..)
import Models exposing (..)
import Tiltak exposing (Tiltak)
import Msgs exposing (Msg(..))
import TiltakAndGroupData
import TiltakView
import Bootstrap.Accordion as Accordion
import Tiltak.LeskurMedSitteplass as LeskurMedSitteplass


gruppePageView : Model -> List Tiltak -> Html Msg
gruppePageView model tiltakene =
    let
        denNyeTypenTiltak =
            LeskurMedSitteplass.tiltak

        tiltakCard tiltak =
            TiltakView.tiltakCard model tiltak

        allCards =
            [ denNyeTypenTiltak ] |> List.map tiltakCard
    in
        Accordion.config AccordionMsg
            |> Accordion.withAnimation
            |> Accordion.cards allCards
            |> Accordion.view model.accordionState


page : Group -> Model -> List (Html Msg)
page tiltaksGruppeType model =
    [ TiltakAndGroupData.tiltakForGroup tiltaksGruppeType |> gruppePageView model ]
