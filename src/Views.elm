module Views exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Bootstrap.Navbar as Navbar
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Card as Card
import Bootstrap.Modal as Modal
import Bootstrap.Accordion as Accordion
import Models exposing (Model, Page(..), Group(..))
import Msgs exposing (Msg(..))
import Group
import TiltakAndGroupData
import Assets
import TiltakView
import Color


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


view : Model -> Html Msg
view model =
    div [ class "contents" ]
        [ mainContent model
        , appFooter
        ]


menuItemLinks : List (Navbar.Item Msg)
menuItemLinks =
    let
        groupToItemLink group =
            Navbar.itemLink
                [ href (Group.groupPath group) ]
                [ text (Group.groupTitle group) ]
    in
        List.map
            groupToItemLink
            TiltakAndGroupData.alleTyper


menu : Navbar.State -> Html Msg
menu navState =
    Navbar.config NavMsg
        |> Navbar.withAnimation
        |> Navbar.inverse
        |> Navbar.darkCustom (Color.rgb 0x3B 0x3B 0x3B)
        |> Navbar.brand
            [ href "#" ]
            [ text "Kollektivkalkulator" ]
        |> Navbar.items menuItemLinks
        |> Navbar.view navState


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
                    , div [] [ text (Group.groupTitle group) ]
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
            [ Grid.container []
                [ h1 [] [ text "Kollektivkalkulator" ]
                , p [] [ text "Nyttekostnadsverktøy for enkle kollektivtiltak" ]
                ]
            ]
        , Grid.container [ class "groupPanels" ]
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
            , Grid.container [] [ tiltakAccordions ]
            ]


pageNotFound : Html Msg
pageNotFound =
    Grid.container []
        [ h1 [] [ text "Ugyldig side" ]
        , text "Beklager, kan ikke finne siden"
        ]


modal : Modal.State -> Html Msg
modal modalState =
    Modal.config ModalMsg
        |> Modal.small
        |> Modal.h4 [] [ text "Getting started ?" ]
        |> Modal.body []
            [ Grid.containerFluid []
                [ Grid.row []
                    [ Grid.col
                        [ Col.xs6 ]
                        [ text "Col 1" ]
                    , Grid.col
                        [ Col.xs6 ]
                        [ text "Col 2" ]
                    ]
                ]
            ]
        |> Modal.view modalState


appFooter : Html Msg
appFooter =
    footer [ class "footer footer-text" ]
        [ Grid.container []
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
            , a [ href "https://www.oslo.kommune.no" ]
                [ img
                    [ Assets.src Assets.byvaapen
                    , class "byvaapen"
                    , alt "Oslo byvåpen"
                    ]
                    []
                ]
            ]
        ]
