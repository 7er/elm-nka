module GroupPage exposing (..)

import Json.Decode as Json
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, defaultOptions, onWithOptions)
import Models exposing (..)
import Msgs exposing (Msg(..), TiltakWidget)
import TiltaksGruppe exposing (..)


renderTiltak : Model -> TiltakWidget -> Html Msg
renderTiltak model tiltak =
    let
        ourOnClick msg =
            onWithOptions "click"
                { defaultOptions | preventDefault = True }
                (Json.succeed msg)
    in
        li []
            (h3 []
                [ a
                    [ href "javascript:void();", ourOnClick (ToggleVisible tiltak) ]
                    [ text tiltak.name ]
                ]
                :: tiltak.page model
            )


gruppePageView : Model -> TiltaksGruppe -> List (Html Msg)
gruppePageView model ({ tiltakene } as tiltaksGruppe) =
    [ ul [] (List.map (renderTiltak model) tiltakene)
    ]


page : TiltaksGruppeType -> Model -> List (Html Msg)
page tiltaksGruppeType model =
    activeGruppe tiltaksGruppeType
        |> Maybe.map (gruppePageView model)
        |> Maybe.withDefault [ h1 [] [ text "Ugyldig gruppe" ] ]
