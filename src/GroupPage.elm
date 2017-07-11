module GroupPage exposing (..)

import Json.Decode as Json
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, defaultOptions, onWithOptions)
import Models exposing (..)
import Msgs exposing (Msg(..))


renderTiltak : Tiltak -> Html Msg
renderTiltak tiltak =
    let
        ourOnClick msg =
            onWithOptions "click" { defaultOptions | preventDefault = True } (Json.succeed msg)

        baseContent =
            [ h3 [] [ a [ href "", ourOnClick (ToggleVisible tiltak) ] [ text tiltak.name ] ] ]

        content =
            case tiltak.visible of
                True ->
                    baseContent ++ [ input [ value tiltak.data ] [] ]

                False ->
                    baseContent
    in
        li [] content


gruppePageView : TiltaksGruppe -> List (Html Msg)
gruppePageView ({ tiltakene } as tiltaksGruppe) =
    [ ul [] (List.map renderTiltak tiltakene)
    ]


page : TiltaksGruppeType -> Model -> List (Html Msg)
page tiltaksGruppeType model =
    activeGruppe tiltaksGruppeType
        |> Maybe.map gruppePageView
        |> Maybe.withDefault [ h1 [] [ text "Ugyldig gruppe" ] ]
