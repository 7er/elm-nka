module Views exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Bootstrap.Navbar as Navbar
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Card as Card
import Bootstrap.Button as Button
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
    Grid.container [] <|
        case model.page of
            Home ->
                pageHome model

            NotFound ->
                pageNotFound

            GroupPage tiltaksGruppeType ->
                pageGroup tiltaksGruppeType model


groupLink group =
    a
        [ href (Group.groupPath group) ]
        [ text (Group.groupTitle group) ]


groupPanel group =
    a
        [ href (Group.groupPath group) ]
        [ Card.config [ Card.outlinePrimary ]
            |> Card.block []
                [ Card.text []
                    [ img [ Assets.src (groupIcon group) ] []
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


pageHome : Model -> List (Html Msg)
pageHome model =
    [ div [ class "bilde_wrapper" ]
        [ img
            [ Assets.src Assets.trikkRikshospitalet
            , class "trikkRikshospitalet"
            , alt "Trikk utenfor Rikshospitalet"
            ]
            []
        ]
    , h1
        [ class "forside__overskrift" ]
        [ text "Kollektivkalkulator" ]
    , p [] [ text "Nyttekostnadsverktøy for enkle kollektivtiltak" ]
    , Grid.row []
        [ Grid.col []
            [ groupPanel Holdeplasser
            ]
        , Grid.col []
            [ Card.config [ Card.outlineDanger ]
                |> Card.block []
                    [ Card.text [] [ groupLink Informasjon ]
                    ]
                |> Card.view
            ]
        ]
    , Grid.row []
        [ Grid.col []
            [ Card.config [ Card.outlinePrimary ]
                |> Card.block []
                    [ Card.text [] [ groupLink Trygghet ]
                    ]
                |> Card.view
            ]
        , Grid.col []
            [ Card.config [ Card.outlineDanger ]
                |> Card.block []
                    [ Card.text [] [ groupLink Kjoeremateriell ]
                    ]
                |> Card.view
            ]
        ]
    , Grid.row []
        [ Grid.col []
            [ Card.config [ Card.outlinePrimary ]
                |> Card.block []
                    [ Card.text [] [ groupLink StrekningOgFramkommelighet ]
                    ]
                |> Card.view
            ]
        ]
    ]


pageGroup : Group -> Model -> List (Html Msg)
pageGroup group model =
    let
        allCards =
            TiltakAndGroupData.tiltakForGroup group
                |> List.map (TiltakView.tiltakCard model)

        header =
            div [ class "groupHeader" ]
                [ a [ href "#" ]
                    [ img
                        [ Assets.src Assets.backArrow
                        , class "backArrow"
                        , alt "Tilbake til forsiden"
                        ]
                        []
                    ]
                ]

        tiltakAccordions =
            Accordion.config AccordionMsg
                |> Accordion.withAnimation
                |> Accordion.cards allCards
                |> Accordion.view model.accordionState
    in
        [ header
        , tiltakAccordions
        ]


pageNotFound : List (Html Msg)
pageNotFound =
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
