module Views exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Bootstrap.Navbar as Navbar
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Card as Card
import Bootstrap.Button as Button
import Bootstrap.Modal as Modal
import Models exposing (..)
import Msgs exposing (Msg)
import GroupPage
import Tiltak
import Msgs exposing (Msg(..))


view : Model -> Html Msg
view model =
    div []
        [ menu model.navState
        , mainContent model
        , modal model.modalState
        ]


menu : Navbar.State -> Html Msg
menu navState =
    let
        groupToItemLink group =
            Navbar.itemLink [ href (Tiltak.tiltaksGruppePath group) ] [ text (Tiltak.tiltaksGruppeTittel group) ]

        itemLinks =
            [ Navbar.itemLink [ href "#getting-started" ] [ text "Komme i gang" ]
            ]
                ++ List.map groupToItemLink Tiltak.tiltaksGrupper
    in
        Navbar.config NavMsg
            |> Navbar.withAnimation
            |> Navbar.container
            |> Navbar.brand [ href "#" ] [ text "TØI NKA-verktøy for kollektivtiltak" ]
            |> Navbar.items itemLinks
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
                GroupPage.page tiltaksGruppeType model


pageHome : Model -> List (Html Msg)
pageHome model =
    [ h1 [] [ text "Hjem" ]
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