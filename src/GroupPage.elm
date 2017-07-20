module GroupPage exposing (..)

import Html exposing (..)
import Models exposing (..)
import Msgs exposing (Msg)
import TiltakAndGroupData
import TiltakView


gruppePageView : Model -> List Tiltak -> List (Html Msg)
gruppePageView model tiltakene =
    let
        tiltakElement tiltak =
            li [] <| TiltakView.tiltakView model tiltak

        allTiltakElements =
            tiltakene |> List.map tiltakElement
    in
        [ ul [] allTiltakElements ]


page : Group -> Model -> List (Html Msg)
page tiltaksGruppeType model =
    TiltakAndGroupData.tiltakForGroup tiltaksGruppeType |> gruppePageView model
