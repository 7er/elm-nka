module Main exposing (main)

import Navigation exposing (Location)
import Bootstrap.Navbar as Navbar
import Bootstrap.Modal as Modal
import Bootstrap.Accordion as Accordion
import UrlParser exposing ((</>))
import Models exposing (..)
import Msgs exposing (Msg(..))
import Group
import Tiltak exposing (Tiltak, sendTo)
import Field exposing (Field)
import TiltakCharting exposing (GraphState(..))
import TiltakStates exposing (TiltakStates)
import TiltakAndGroupData
import Views exposing (view)
import Ports


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

        model =
            { page = pageFromLocation location
            , navState = navState
            , modalState = Modal.hiddenState
            , accordionState = Accordion.initialState
            , tiltakStates = TiltakAndGroupData.initialTiltakStates
            , chartIds = []
            }
    in
        -- if we had more than one cmd, use Cmd.batch : List Cmd -> Cmd
        ( model, navCmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Navbar.subscriptions model.navState NavMsg
        , Accordion.subscriptions model.accordionState AccordionMsg
        , Ports.charts ChartsChanged
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            let
                newModel =
                    { model | page = pageFromLocation location }
            in
                ( newModel
                , destroyAndCreateCharts model newModel
                )

        NavMsg state ->
            ( { model | navState = state }, Cmd.none )

        ModalMsg state ->
            ( { model | modalState = state }, Cmd.none )

        AccordionMsg state ->
            ( { model | accordionState = state }, Cmd.none )

        UpdateField tiltak field stringValue ->
            updateField model tiltak field stringValue

        UpdateBooleanField field booleanValue ->
            Debug.log (toString msg) ( model, Cmd.none )

        ChartsChanged chartIds ->
            ( { model | chartIds = chartIds }, Cmd.none )


destroyAndCreateCharts : Model -> Model -> Cmd msg
destroyAndCreateCharts oldModel newModel =
    Cmd.batch
        [ case oldModel.page of
            GroupPage group ->
                groupDestroyCharts group oldModel

            _ ->
                Cmd.none
        , case newModel.page of
            GroupPage group ->
                groupCreateCharts group newModel

            _ ->
                Cmd.none
        ]


groupDestroyCharts : Group -> Model -> Cmd msg
groupDestroyCharts group model =
    TiltakAndGroupData.tiltakForGroup group
        |> List.filter
            (\tiltak ->
                TiltakCharting.graphState tiltak model.tiltakStates == GraphOn
            )
        |> List.map (\tiltak -> sendTo tiltak .graphId)
        |> List.map Ports.destroyC3
        |> Cmd.batch


groupCreateCharts : Group -> Model -> Cmd msg
groupCreateCharts group model =
    TiltakAndGroupData.tiltakForGroup group
        |> List.filter
            (\tiltak ->
                TiltakCharting.graphState tiltak model.tiltakStates == GraphOn
            )
        |> List.map (\tiltak -> TiltakCharting.chartRecord tiltak model.tiltakStates)
        |> List.map Ports.generateC3
        |> Cmd.batch


pageFromLocation : Navigation.Location -> Page
pageFromLocation location =
    decode location |> Maybe.withDefault NotFound


computeGraphCmd : Tiltak -> TiltakStates -> ( GraphState, GraphState ) -> Cmd Msg
computeGraphCmd tiltak tiltakStates ( beforeState, afterState ) =
    let
        graphId =
            sendTo tiltak .graphId

        graphData =
            { domId = graphId
            , data = TiltakCharting.graphData tiltak tiltakStates
            , variableTitle =
                TiltakCharting.findVariableToGraph tiltak tiltakStates
                    |> Maybe.map .title
                    |> Maybe.withDefault "WAT!!!!"
            }
    in
        case ( beforeState, afterState ) of
            ( GraphOff, GraphOn ) ->
                Ports.generateC3 graphData

            ( GraphOff, GraphOff ) ->
                Cmd.none

            ( GraphOn, GraphOn ) ->
                -- generate command to update the graph
                Ports.updateC3 graphData

            ( GraphOn, GraphOff ) ->
                Ports.destroyC3 graphId


updateField : Model -> Tiltak -> Field -> String -> ( Model, Cmd Msg )
updateField model tiltak field stringValue =
    let
        oldGraphState =
            TiltakCharting.graphState tiltak model.tiltakStates

        newGraphState =
            TiltakCharting.graphState tiltak newTiltakStates

        newTiltakStates =
            Field.updateTiltakStateFromField
                field
                stringValue
                model.tiltakStates
    in
        ( { model
            | tiltakStates = newTiltakStates
          }
        , computeGraphCmd
            tiltak
            newTiltakStates
            ( oldGraphState
            , newGraphState
            )
        )


decode : Location -> Maybe Page
decode location =
    UrlParser.parseHash routeParser location


routeParser : UrlParser.Parser (Page -> a) a
routeParser =
    let
        groupToParser group =
            UrlParser.map (GroupPage group) <|
                UrlParser.s <|
                    Group.groupPathSansHash group
    in
        TiltakAndGroupData.alleTyper
            |> List.map groupToParser
            |> List.append
                [ UrlParser.map Home UrlParser.top
                ]
            |> UrlParser.oneOf
