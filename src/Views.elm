module Views exposing (..)

import Json.Decode as Json
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, defaultOptions, onWithOptions)
import Msgs exposing (Msg(..))
import Models exposing (..)


groupTitle : TiltaksGruppe -> String
groupTitle { tag } =
    toString tag


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
                    baseContent ++ [ input [ onInput (UpdateData tiltak), value tiltak.data ] [] ]

                False ->
                    baseContent
    in
        li [] content


renderGruppe : TiltaksGruppe -> Html Msg
renderGruppe { tiltakene } =
    ul [] (List.map renderTiltak tiltakene)


renderNav : Model -> Html Msg
renderNav model =
    let
        groupToNavLi tiltaksGruppe =
            li [] [ a [ href (tiltaksGruppePath tiltaksGruppe) ] [ text (tiltaksGruppeTittel tiltaksGruppe) ] ]
    in
        ul [] <| List.map groupToNavLi model.tiltaksGrupper


gruppePageView tiltaksGruppe =
    [ div [] [ text <| "Valgt gruppe er: " ++ (groupTitle tiltaksGruppe) ]
    , renderGruppe tiltaksGruppe
    ]


view : Model -> Html Msg
view model =
    div []
        [ nav [] [ renderNav model ]
        , article []
            (case model.route of
                GruppeRoute typeTag ->
                    case activeGruppe model typeTag of
                        Just tiltaksGruppe ->
                            gruppePageView tiltaksGruppe

                        Nothing ->
                            [ text "Ugyldig gruppe" ]

                Root ->
                    [ text "Slike ting skal ikke skje" ]

                NotFoundRoute ->
                    [ text "Ugyldige side" ]
            )
        ]
