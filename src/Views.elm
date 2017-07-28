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


view : Model -> Html Msg
view model =
    div [ class "contents" ]
        [ menu model.navState
        , mainContent model
        , modal model.modalState
        , appFooter
        ]


menuItemLinks : List (Navbar.Item Msg)
menuItemLinks =
    let
        groupToItemLink group =
            Navbar.itemLink
                [ href (Group.groupPath group) ]
                [ text (Group.groupTitle group) ]

        gettingStarted =
            Navbar.itemLink
                [ href "#getting-started" ]
                [ text "Komme i gang" ]
    in
        gettingStarted
            :: List.map
                groupToItemLink
                TiltakAndGroupData.alleTyper


menu : Navbar.State -> Html Msg
menu navState =
    Navbar.config NavMsg
        |> Navbar.withAnimation
        |> Navbar.container
        |> Navbar.brand
            [ href "#" ]
            [ text "TØI Kollektivtrafikk tiltak kalkulator" ]
        |> Navbar.items menuItemLinks
        |> Navbar.view navState


mainContent : Model -> Html Msg
mainContent model =
    Grid.container [] <|
        case model.page of
            Home ->
                pageHome model

            GettingStarted ->
                pageGettingStarted model

            NotFound ->
                pageNotFound

            GroupPage tiltaksGruppeType ->
                pageGroup tiltaksGruppeType model


pageHome : Model -> List (Html Msg)
pageHome model =
    [ h1 [] [ text "TØI Kollektivtrafikk kalkulator" ]
    , p [] [ text "Kalkulatorer for kostnad-nytte-analyse av tiltak for kollektivtrafikk" ]
    , Grid.row []
        [ Grid.col []
            [ Card.config [ Card.outlinePrimary ]
                |> Card.headerH4 [] [ text "Komme i gang" ]
                |> Card.block []
                    [ Card.text [] [ text "Hvordan komme i gang" ]
                    , Card.custom <|
                        Button.linkButton
                            [ Button.primary, Button.attrs [ href "#getting-started" ] ]
                            [ text "Start" ]
                    ]
                |> Card.view
            ]
        , Grid.col []
            [ Card.config [ Card.outlineDanger ]
                |> Card.headerH4 [] [ text "Underlagsmateriale" ]
                |> Card.block []
                    [ Card.text [] [ text "Bla bla om materiale som verktøyet baserer seg på" ]
                    , Card.custom <|
                        Button.linkButton
                            [ Button.primary, Button.attrs [ href "http://www.toi.no" ] ]
                            [ text "Underlag" ]
                    ]
                |> Card.view
            ]
        ]
    ]


pageGettingStarted : Model -> List (Html Msg)
pageGettingStarted model =
    [ h2 [] [ text "Komme i gang" ]
    , Button.button
        [ Button.success
        , Button.large
        , Button.block
        , Button.attrs [ onClick <| ModalMsg Modal.visibleState ]
        ]
        [ text "Trykk meg" ]
    ]


pageGroup : Group -> Model -> List (Html Msg)
pageGroup tiltaksGruppeType model =
    let
        tiltakene =
            TiltakAndGroupData.tiltakForGroup tiltaksGruppeType

        allCards =
            List.map (TiltakView.tiltakCard model) tiltakene
    in
        [ Accordion.config AccordionMsg
            |> Accordion.withAnimation
            |> Accordion.cards allCards
            |> Accordion.view model.accordionState
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
    footer [ class "footer" ]
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
