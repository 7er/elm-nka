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
import Msgs exposing (Msg(..))
import Group
import Assets


view : Model -> Html Msg
view model =
    div [class "contents" ]
        [ menu model.navState
        , mainContent model
        , modal model.modalState
        , appFooter
        ]


menu : Navbar.State -> Html Msg
menu navState =
    let
        groupToItemLink group =
            Navbar.itemLink [ href (Group.tiltaksGruppePath group) ] [ text (Group.tiltaksGruppeTittel group) ]

        itemLinks =
            [ Navbar.itemLink [ href "#getting-started" ] [ text "Komme i gang" ]
            ]
                ++ List.map groupToItemLink Group.alleTyper
    in
        Navbar.config NavMsg
            |> Navbar.withAnimation
            |> Navbar.container
            |> Navbar.brand [ href "#" ] [ text "TØI Kollektivtrafikk tiltak kalkulator" ]
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
    [ h1 [ class "overskrift"] [ text "TØI Kollektivtrafikk kalkulator" ]
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
    footer [] [
        div [] [ text "Kontakt: "
            , a [href "mailto:naf@toi.no"] [text "Nils Fearnley"]
            , br [] []
            , a [href "https://www.toi.no"] [
                img [Assets.src Assets.toiLogo, class "toiLogo", alt "TØI logo"] [] ]
            , a [href "https://www.oslo.kommune.no"] [
                img [Assets.src Assets.byvaapen, class "byvaapen", alt "Oslo byvåpen"] [] ]
        ]
    ]
