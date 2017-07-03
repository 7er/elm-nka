port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, defaultOptions, onWithOptions)
import Json.Decode as Json
import Navigation exposing (Location)
import Models exposing (..)


-- MAIN


main =
    Navigation.program
        OnLocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


port printMediaType : (Bool -> msg) -> Sub msg



-- MESSAGES


type Msg
    = NoOp
    | Update String
    | MediaTypeChanged Bool
    | ToggleVisible Tiltak
    | UpdateData Tiltak String
    | OnLocationChange Location



-- UPDATE


init : Location -> ( Model, Cmd Msg )
init location =
    let
        model =
            { message = "Hello"
            , printMediaType = False
            , tiltaksGrupper =
                [ { tag = Holdeplasser
                  , tiltakene =
                        [ createTiltak "Sykkelparkering" "Foobar"
                        , createTiltak "Leskur u sitteplass" "Zppt"
                        , createTiltak "Sitteplass på hpl" "Syver"
                        ]
                  }
                , { tag = Informasjon
                  , tiltakene =
                        [ createTiltak "Skilting i buss" "Foobar"
                        , createTiltak "Hpl. opprop" "Zppt"
                        ]
                  }
                ]
            , route = routeFromLocation location
            }
    in
        ( model
        , case model.route of
            Root ->
                Navigation.modifyUrl (location.href ++ "#holdeplasser")

            _ ->
                Cmd.none
        )


routeFromLocation : Location -> Route
routeFromLocation location =
    case location.hash of
        "" ->
            Root

        "#holdeplasser" ->
            GruppeRoute
                { tag = Holdeplasser
                , tiltakene =
                    [ createTiltak "Sykkelparkering" "Foobar"
                    , createTiltak "Leskur u sitteplass" "Zppt"
                    , createTiltak "Sitteplass på hpl" "Syver"
                    ]
                }

        _ ->
            NotFoundRoute


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "update: " msg of
        NoOp ->
            ( model, Cmd.none )

        Update inputString ->
            ( { model | message = inputString }, Cmd.none )

        MediaTypeChanged isPrintType ->
            ( { model | printMediaType = isPrintType }, Cmd.none )

        ToggleVisible tiltak ->
            ( { model | route = toggleTiltak model.route tiltak }, Cmd.none )

        UpdateData tiltak newData ->
            ( { model | route = updateData model.route tiltak newData }, Cmd.none )

        OnLocationChange location ->
            ( { model | route = routeFromLocation location }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    printMediaType MediaTypeChanged



-- VIEW


groupTitle : Model -> Html Msg
groupTitle { route } =
    case route of
        GruppeRoute tiltaksGruppe ->
            text (toString tiltaksGruppe.tag)

        NotFoundRoute ->
            text "Finner ikke siden"

        Root ->
            text "Rotsiden"


renderTiltak : Tiltak -> Html Msg
renderTiltak tiltak =
    let
        ourOnClick msg =
            onWithOptions "click" { defaultOptions | preventDefault = True } (Json.succeed msg)

        baseContent =
            [ h3 [] [ a [ href "", ourOnClick (ToggleVisible tiltak) ] [ text tiltak.name ] ] ]

        content =
            case tiltak.visible of
                True ->
                    baseContent ++ [ input [ onInput (UpdateData tiltak), value tiltak.data ] [] ]

                False ->
                    baseContent
    in
        li [] content


renderGruppe : TiltaksGruppe -> Html Msg
renderGruppe { tiltakene } =
    ul [] (List.map renderTiltak tiltakene)


renderTiltakene : Model -> Html Msg
renderTiltakene { route } =
    case route of
        GruppeRoute gruppe ->
            renderGruppe gruppe

        NotFoundRoute ->
            text "Ikke tilgjengelig"

        Root ->
            text "Er det MULIG??"


renderNav : Model -> Html Msg
renderNav model =
    let
        groupToNavLi tiltaksGruppe =
            li [] [ a [ href (tiltaksGruppePath tiltaksGruppe) ] [ text (tiltaksGruppeTittel tiltaksGruppe) ] ]
    in
        ul [] <| List.map groupToNavLi model.tiltaksGrupper


view : Model -> Html Msg
view model =
    div []
        [ nav [] [ renderNav model ]
        , article []
            [ div [] [ groupTitle model ]
            , text model.message
            , input [ onInput Update ] []

            --        , div [] [text ("is print " ++ (toString model.printMediaType))]
            , renderTiltakene model
            ]
        ]
