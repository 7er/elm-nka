module Views exposing (view)

import Assets
import Bootstrap.Accordion as Accordion
import Bootstrap.Card as Card
import Bootstrap.Grid as Grid
import Group
import Html exposing (..)
import Html.Attributes exposing (..)
import Models exposing (Group(..), Model, Page(..))
import Msgs exposing (Msg(..))
import TiltakAndGroupData
import TiltakView


groupIcon : Group -> Assets.Path
groupIcon group =
    case group of
        Holdeplasser ->
            Assets.holdeplasser

        Informasjon ->
            Assets.informasjon

        Trygghet ->
            Assets.trygghet

        Kjoeremateriell ->
            Assets.kjoeremateriell

        StrekningOgFramkommelighet ->
            Assets.strekningOgFramkommelighet

        Tilgjengelighet ->
            Assets.tilgjengelighet


view : Model -> Html Msg
view model =
    div [ class "contents" ]
        [ mainContent model
        , appFooter
        ]


mainContent : Model -> Html Msg
mainContent model =
    case model.page of
        Home ->
            pageHome model

        NotFound ->
            pageNotFound

        GroupPage tiltaksGruppeType ->
            pageGroup tiltaksGruppeType model


groupPanel : Group -> Html Msg
groupPanel group =
    a
        [ href (Group.groupPath group)
        , class "groupPanel"
        ]
        [ Card.config []
            |> Card.block []
                [ Card.text []
                    [ img
                        [ class "groupIcon"
                        , Assets.src (groupIcon group)
                        , alt ""
                        ]
                        []
                    , div
                        [ class "group-box-title" ]
                        [ div
                            [ class "group-box-title-text" ]
                            [ text (Group.groupTitle group) ]
                        ]
                    , img
                        [ Assets.src Assets.caretRight
                        , class "caretRight"
                        , alt ""
                        ]
                        []
                    ]
                ]
            |> Card.view
        ]


pageHome : Model -> Html Msg
pageHome model =
    div []
        [ div [ class "jumbotron homeHeader" ]
            [ Grid.container [ class "container__narrow" ]
                [ h1 [] [ text "Kollektivkalkulator" ]
                , h2 [] [ text "Nyttekostnadsverktøy for enkle kollektivtiltak" ]
                , p [] [ text "Publisert 2020" ]
                ]
            ]
        , Grid.container [ class "groupPanels container__narrow" ]
            [ div [ class "forsidetekst" ]
                [ Grid.row []
                    [ Grid.col []
                        [ p []
                            [ text """
Kollektivkalkulatoren er et
nyttekostnadsberegningsverktøy for
små og enkeltvise kollektivtransporttiltak. Kalkulatoren følger gjeldende
tilnærming og metodikk for nyttekostnadsanalyser i
transportsektoren. Derfor kan NKA-beregningene sammenlignes med andre
samferdselstiltak.
"""
                            ]
                        , p []
                            [ text """
Velg hovedkategori av tiltak fra boksene nedenfor, og deretter konkret tiltak.
Ved å legge inn bakgrunnsinformasjon om prosjektet, beregner kalkulatoren nytte for
ulike aktører, tiltakets nettonåverdi og nettonytte per budsjettkrone (nyttekostnadsbrøk).
"""
                            ]
                        , p []
                            [ text "Beregningsopplegget er dokumentert i "
                            , a
                                [ target "_blank"
                                , href "https://www.toi.no/publikasjoner/article29858-8.html"
                                ]
                                [ text "TØI-rapport 1121" ]
                            , text
                                ". Her fins også nærmere veiledning til utfylling og bruk av kalkulatoren, samt erfaringsbaserte anslag på tiltakenes kostnader."
                            , text " I 2020 ble verktøyet oppdatert med nye inngangsverdier. Dette er dokumentert i "
                            , a
                                [ target "_blank"
                                , Assets.href Assets.arbeidsdokument51690
                                ]
                                [ text "arbeidsdokument 51690" ]
                            , text "."
                            ]
                        ]
                    ]
                ]
            , Grid.row []
                [ Grid.col []
                    [ groupPanel Holdeplasser
                    ]
                , Grid.col []
                    [ groupPanel Informasjon
                    ]
                ]
            , Grid.row []
                [ Grid.col []
                    [ groupPanel Trygghet ]
                , Grid.col []
                    [ groupPanel Kjoeremateriell ]
                ]
            , Grid.row []
                [ Grid.col []
                    [ groupPanel StrekningOgFramkommelighet ]
                , Grid.col []
                    [ groupPanel Tilgjengelighet ]
                ]
            ]
        ]


pageGroup : Group -> Model -> Html Msg
pageGroup group model =
    let
        allCards =
            TiltakAndGroupData.tiltakForGroup group
                |> List.map (TiltakView.tiltakCard model.tiltakStates)

        pageHeader =
            header [ class "groupHeader" ]
                [ a [ href "#" ]
                    [ img
                        [ Assets.src Assets.backArrow
                        , class "backArrow"
                        , alt "Tilbake til forsiden"
                        ]
                        []
                    ]
                , div [ class "groupPageHeader" ]
                    [ img
                        [ class "groupIcon"
                        , Assets.src (groupIcon group)
                        , alt ""
                        ]
                        []
                    ]
                , h1 [] [ text (Group.groupTitle group) ]
                ]

        tiltakAccordions =
            Accordion.config AccordionMsg
                |> Accordion.withAnimation
                |> Accordion.cards allCards
                |> Accordion.view model.accordionState
    in
    div []
        [ Grid.containerFluid [] [ pageHeader ]
        , Grid.container [ class "container__narrow" ] [ tiltakAccordions ]
        ]


pageNotFound : Html Msg
pageNotFound =
    Grid.container [ class "container__narrow" ]
        [ h1 [] [ text "Ugyldig side" ]
        , text "Beklager, kan ikke finne siden"
        ]


appFooter : Html Msg
appFooter =
    footer [ class "footer footer-text" ]
        [ Grid.container [ class "container__narrow" ]
            [ text "Kontakt: "
            , a [ href "mailto:naf@toi.no" ] [ text "Nils Fearnley" ]
            , br [] []
            , a [ href "https://www.toi.no" ]
                [ img
                    [ Assets.src Assets.toiLogo
                    , class "toiLogo"
                    , alt "TØI logo"
                    ]
                    []
                ]
            , div [ class "colophon" ]
                [ text "Utvikling og design: "
                , a [ href "http://www.72web.no" ]
                    [ text "72web.no" ]
                , text " ved Syver Enstad & Thomas Flemming"
                ]
            ]
        ]
