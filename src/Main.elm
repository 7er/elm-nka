module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Navigation exposing (Location)
import UrlParser exposing ((</>))
import Bootstrap.Navbar as Navbar
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Card as Card
import Bootstrap.Button as Button
import Bootstrap.Modal as Modal
import PageSykkelparkeringUte
import PageSeparatSykkelveg
import GroupPage
import Models exposing (..)
import Msgs exposing (Msg(..))


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

        FieldUpdate updateFunc stringValue ->
            ( (updateFunc stringValue model), Cmd.none )

        FormSubmit submitFunc ->
            submitFunc model

        ToggleVisible tiltak ->
            Debug.crash "TODO"


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    case decode location of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just route ->
            ( { model | page = route }, Cmd.none )


decode : Location -> Maybe Page
decode location =
    case UrlParser.parseHash routeParser location of
        (Just page) as result ->
            result

        Nothing ->
            gruppeFromHash location.hash |> Maybe.map GroupPage


routeParser : UrlParser.Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Home UrlParser.top
        , UrlParser.map GettingStarted (UrlParser.s "getting-started")
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
    let
        groupToItemLink group =
            Navbar.itemLink [ href (tiltaksGruppePath group) ] [ text (tiltaksGruppeTittel group) ]

        itemLinks =
            [ Navbar.itemLink [ href "#getting-started" ] [ text "Komme i gang" ]
            , Navbar.itemLink [ href "#sykkelparkering-ute" ] [ text "Sykkelparkering ute" ]
            , Navbar.itemLink [ href "#separat-sykkelveg" ] [ text "Separat sykkelveg" ]
            ]
                ++ List.map groupToItemLink tiltaksGrupper
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

            SykkelparkeringUte ->
                PageSykkelparkeringUte.page model

            SeparatSykkelveg ->
                PageSeparatSykkelveg.page model

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
