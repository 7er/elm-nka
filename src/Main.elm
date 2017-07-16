module Main exposing (main)

import Navigation exposing (Location)
import Bootstrap.Navbar as Navbar
import Bootstrap.Modal as Modal
import UrlParser exposing ((</>))
import Models exposing (..)
import Msgs exposing (Msg(..))
import Tiltak
import Field exposing (..)
import Views exposing (view)
import TiltakComponents.SykkelparkeringUte
import TiltakComponents.SeparatSykkelveg
import Tiltak


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
            , tiltakStates =
                { sykkelParkeringUteTiltakState = TiltakComponents.SykkelparkeringUte.initialTiltakState
                , separatSykkelvegTiltakState = TiltakComponents.SeparatSykkelveg.initialTiltakState
                , leskurUtenSitteplassTiltakState = createTiltakState {}
                , skiltingIBussTiltakState = createTiltakState {}
                }
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
            ( { model | tiltakStates = (updateFunc stringValue model.tiltakStates) }, Cmd.none )

        FormSubmit submitFunc ->
            submitFunc model

        ToggleVisible tiltakObject ->
            ( { model | tiltakStates = tiltakObject.toggleVisible model.tiltakStates }, Cmd.none )


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
            Tiltak.gruppeFromHash location.hash |> Maybe.map GroupPage


routeParser : UrlParser.Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Home UrlParser.top
        , UrlParser.map GettingStarted (UrlParser.s "getting-started")
        ]
