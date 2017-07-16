module GroupPage exposing (..)

import Json.Decode as Json
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, defaultOptions, onWithOptions)
import Models exposing (..)
import Msgs exposing (Msg(..), TiltakObject)
import TiltaksGruppe exposing (..)


tiltakView : Model -> TiltakObject -> Html Msg
tiltakView model tiltak =
    let
        ourOnClick msg =
            onWithOptions "click"
                { defaultOptions | preventDefault = True }
                (Json.succeed msg)

        baseContent =
            h3 []
                [ a
                    [ href "javascript:void();", ourOnClick (ToggleVisible tiltak) ]
                    [ text tiltak.name ]
                ]

        maybePage =
            case tiltak.isVisible model of
                True ->
                    tiltak.page model

                False ->
                    []
    in
        [ baseContent ]
            ++ maybePage
            |> li []


gruppePageView : Model -> TiltaksGruppe -> List (Html Msg)
gruppePageView model ({ tiltakene } as tiltaksGruppe) =
    [ ul [] (List.map (tiltakView model) tiltakene)
    ]


page : TiltaksGruppeType -> Model -> List (Html Msg)
page tiltaksGruppeType model =
    activeGruppe tiltaksGruppeType
        |> Maybe.map (gruppePageView model)
        |> Maybe.withDefault [ h1 [] [ text "Ugyldig gruppe" ] ]
