module Main exposing (main)

import Navigation exposing (Location)
import Bootstrap.Navbar as Navbar
import Bootstrap.Modal as Modal
import Bootstrap.Accordion as Accordion
import UrlParser exposing ((</>))
import Models exposing (..)
import Msgs exposing (Msg(..))
import TiltakStates exposing (TiltakStates)
import Views exposing (view)
import Group
import TiltakAndGroupData


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
            { page = Home
            , navState = navState
            , modalState = Modal.hiddenState
            , accordionState = Accordion.initialState
            , tiltakStates = TiltakAndGroupData.initialTiltakStates
            }

        model =
            urlUpdate location initialModel
    in
        -- if we had more than one cmd, use Cmd.batch : List Cmd -> Cmd
        ( model, navCmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Navbar.subscriptions model.navState NavMsg
        , Accordion.subscriptions model.accordionState AccordionMsg
        ]


updateTiltakStateFromField : Field -> String -> TiltakStates -> TiltakStates
updateTiltakStateFromField field stringValue tiltakStates =
    field.updateTiltakState stringValue tiltakStates


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            ( urlUpdate location model, Cmd.none )

        NavMsg state ->
            ( { model | navState = state }, Cmd.none )

        ModalMsg state ->
            ( { model | modalState = state }, Cmd.none )

        AccordionMsg state ->
            ( { model | accordionState = state }, Cmd.none )

        UpdateField field stringValue ->
            ( { model
                | tiltakStates =
                    updateTiltakStateFromField
                        field
                        stringValue
                        model.tiltakStates
              }
            , Cmd.none
            )


urlUpdate : Navigation.Location -> Model -> Model
urlUpdate location model =
    case decode location of
        Nothing ->
            { model | page = NotFound }

        Just route ->
            { model | page = route }


decode : Location -> Maybe Page
decode location =
    case UrlParser.parseHash routeParser location of
        (Just page) as result ->
            result

        Nothing ->
            Group.gruppeFromHash location.hash |> Maybe.map GroupPage


routeParser : UrlParser.Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Home UrlParser.top
        , UrlParser.map GettingStarted (UrlParser.s "getting-started")
        ]
