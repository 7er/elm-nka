module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import ModelAndMsg exposing (..)
import Navigation exposing (Location)
import UrlParser exposing ((</>))
import Bootstrap.Navbar as Navbar
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Card as Card
import Bootstrap.Button as Button
import Bootstrap.ListGroup as Listgroup
import Bootstrap.Modal as Modal
import PageSykkelparkeringUte
import PageSeparatSykkelveg


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { view = view
        , update = update
        , subscriptions = subscriptions
        , init = init
        }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        ( navState, navCmd ) =
            Navbar.initialState NavMsg

        initialModel =
            { navState = navState
            , page = Home
            , modalState = Modal.hiddenState
            , sykkelParkeringUteFormState = PageSykkelparkeringUte.initialFormState
            , separatSykkelvegFormState = PageSeparatSykkelveg.initialFormState
            }

        ( model, urlCmd ) =
            urlUpdate location initialModel
    in
        ( model, Cmd.batch [ urlCmd, navCmd ] )


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navState NavMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            urlUpdate location model

        NavMsg state ->
            ( { model | navState = state }
            , Cmd.none
            )

        ModalMsg state ->
            ( { model | modalState = state }
            , Cmd.none
            )

        SykkelparkeringUteSubmit ->
            let
                oldState =
                    model.sykkelParkeringUteFormState

                newState =
                    { oldState | submitted = True }
            in
                ( { model | sykkelParkeringUteFormState = newState }, PageSykkelparkeringUte.loadGraph )

        SykkelparkeringUteForm variableName stringValue ->
            ( { model
                | sykkelParkeringUteFormState =
                    PageSykkelparkeringUte.updateFormState model.sykkelParkeringUteFormState variableName stringValue
              }
            , Cmd.none
            )

        SeparatSykkelvegSubmit ->
            let
                oldState =
                    model.separatSykkelvegFormState

                newState =
                    { oldState | submitted = True }
            in
                ( { model | separatSykkelvegFormState = newState }, PageSeparatSykkelveg.loadGraph )

        SeparatSykkelvegForm variableName stringValue ->
            ( { model
                | separatSykkelvegFormState =
                    PageSeparatSykkelveg.updateFormState model.separatSykkelvegFormState variableName stringValue
              }
            , Cmd.none
            )


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    case decode location of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just route ->
            ( { model | page = route }, Cmd.none )


decode : Location -> Maybe Page
decode location =
    UrlParser.parseHash routeParser location


routeParser : UrlParser.Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Home UrlParser.top
        , UrlParser.map GettingStarted (UrlParser.s "getting-started")
        , UrlParser.map Modules (UrlParser.s "modules")
        , UrlParser.map SykkelparkeringUte (UrlParser.s "sykkelparkering-ute")
        , UrlParser.map SeparatSykkelveg (UrlParser.s "separat-sykkelveg")
        ]


view : Model -> Html Msg
view model =
    div []
        [ menu model.navState
        , mainContent model
        , modal model.modalState
        ]


menu : Navbar.State -> Html Msg
menu navState =
    Navbar.config NavMsg
        |> Navbar.withAnimation
        |> Navbar.container
        |> Navbar.brand [ href "#" ] [ text "TØI NKA-verktøy for kollektivtiltak" ]
        |> Navbar.items
            [ Navbar.itemLink [ href "#getting-started" ] [ text "Getting started" ]
            , Navbar.itemLink [ href "#modules" ] [ text "Modules" ]
            , Navbar.itemLink [ href "#sykkelparkering-ute" ] [ text "Sykkelparkering ute" ]
            , Navbar.itemLink [ href "#separat-sykkelveg" ] [ text "Separat sykkelveg" ]
            ]
        |> Navbar.view navState


mainContent : Model -> Html Msg
mainContent model =
    Grid.container [] <|
        case model.page of
            Home ->
                pageHome model

            GettingStarted ->
                pageGettingStarted model

            Modules ->
                pageModules model

            NotFound ->
                pageNotFound

            SykkelparkeringUte ->
                PageSykkelparkeringUte.page model

            SeparatSykkelveg ->
                PageSeparatSykkelveg.page model


pageHome : Model -> List (Html Msg)
pageHome model =
    [ h1 [] [ text "Home" ]
    , Grid.row []
        [ Grid.col []
            [ Card.config [ Card.outlinePrimary ]
                |> Card.headerH4 [] [ text "Getting started" ]
                |> Card.block []
                    [ Card.text [] [ text "Getting started is real easy. Just click the start button." ]
                    , Card.custom <|
                        Button.linkButton
                            [ Button.primary, Button.attrs [ href "#getting-started" ] ]
                            [ text "Start" ]
                    ]
                |> Card.view
            ]
        , Grid.col []
            [ Card.config [ Card.outlineDanger ]
                |> Card.headerH4 [] [ text "Modules" ]
                |> Card.block []
                    [ Card.text [] [ text "Check out the modules overview" ]
                    , Card.custom <|
                        Button.linkButton
                            [ Button.primary, Button.attrs [ href "#modules" ] ]
                            [ text "Module" ]
                    ]
                |> Card.view
            ]
        ]
    ]


pageGettingStarted : Model -> List (Html Msg)
pageGettingStarted model =
    [ h2 [] [ text "Getting started" ]
    , Button.button
        [ Button.success
        , Button.large
        , Button.block
        , Button.attrs [ onClick <| ModalMsg Modal.visibleState ]
        ]
        [ text "Click me" ]
    ]


pageModules : Model -> List (Html Msg)
pageModules model =
    [ h1 [] [ text "Modules" ]
    , Listgroup.ul
        [ Listgroup.li [] [ text "Alert" ]
        , Listgroup.li [] [ text "Badge" ]
        , Listgroup.li [] [ text "Card" ]
        ]
    ]


pageNotFound : List (Html Msg)
pageNotFound =
    [ h1 [] [ text "Not found" ]
    , text "SOrry couldn't find that page"
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
