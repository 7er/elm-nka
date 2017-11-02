module Views exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Bootstrap.Grid as Grid
import Bootstrap.Card as Card
import Bootstrap.Accordion as Accordion
import Models exposing (Model, Page(..), Group(..))
import Msgs exposing (Msg(..))
import Group
import TiltakAndGroupData
import Assets
import TiltakView


groupIcon : Group -> Assets.Image
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
                        ]
                        []
                    , div
                        [class "group-box-title"]
                        [ div
                            [ class "group-box-title-text"]
                            [ text (Group.groupTitle group)]
                        ]
                    , img
                        [ Assets.src Assets.caretRight
                        , class "caretRight"
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
                , p [] [ text "Nyttekostnadsverktøy for enkle kollektivtiltak" ]
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
enklere kollektivtransporttiltak. Kalkulatoren følger gjeldende
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
                    , p [] [ text """
Kalkulatoren viser ved hjelp av en graf hvordan tiltakets nettonåverdi
varierer med nivået på en valgt forutsetning. På den måten kan
kalkulatoren brukes selv om ikke alle forutsetninger er kjent.
""" ]
                    , p []
                        [ text "Beregningsopplegget er dokumentert i "
                        , a [ href "https://www.toi.no/publikasjoner/article29858-8.html" ]
                            [ text "TØI-rapport 1121" ]
                        , text
                            ". Her fins også nærmere veiledning til utfylling og bruk av kalkulatoren."
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
                |> List.map (TiltakView.tiltakCard model)

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
             , div [class "colophon" ] [ text "Utvikling og design Syver Enstad & Thomas Flemming" ]
            ]
        ]
