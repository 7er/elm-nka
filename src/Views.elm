module Views exposing (..)

import Json.Decode as Json
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, defaultOptions, onWithOptions)
import Msgs exposing (Msg(..))
import Models exposing (..)


groupTitle : Model -> Html Msg
groupTitle { route } =
    case route of
        GruppeRoute tag ->
            text (toString tag)

        NotFoundRoute ->
            text "Finner ikke siden"

        Root ->
            text "Rotsiden"


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


renderTiltakene : Model -> Html Msg
renderTiltakene model =
    case model.route of
        GruppeRoute tag ->
            case activeGruppe model tag of
                Just gruppe ->
                    renderGruppe gruppe

                Nothing ->
                    text "Gruppen er ikke lagt til"

        NotFoundRoute ->
            text "Ikke tilgjengelig"

        Root ->
            text "Er det MULIG??"


renderNav : Model -> Html Msg
renderNav model =
    let
        groupToNavLi tiltaksGruppe =
            li [] [ a [ href (tiltaksGruppePath tiltaksGruppe) ] [ text (tiltaksGruppeTittel tiltaksGruppe) ] ]
    in
        ul [] <| List.map groupToNavLi model.tiltaksGrupper


gruppePageView model =
    [ div [] [ groupTitle model ]
    , text model.message
    , input [ onInput Update ] []
    , renderTiltakene model
    ]


view : Model -> Html Msg
view model =
    div []
        [ nav [] [ renderNav model ]
        , article []
            (case model.route of
                GruppeRoute typeTag ->
                    gruppePageView model

                Root ->
                    [ text "Slike ting skal ikke skje" ]

                NotFoundRoute ->
                    [ text "Ugyldige side" ]
            )
        ]
