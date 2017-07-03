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
    ( { message = "Hello"
      , printMediaType = False
      , tiltaksGrupper = []
      , route =
            GruppeSide
                { tag = Holdeplasser
                , tiltakene =
                    [ createTiltak "Sykkelparkering" "Foobar"
                    , createTiltak "Leskur u sitteplass" "Zppt"
                    , createTiltak "Sitteplass på hpl" "Syver"
                    ]
                }
      }
    , Cmd.none
    )


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
            ( toggleTiltak model tiltak, Cmd.none )

        UpdateData tiltak newData ->
            ( updateData model tiltak newData, Cmd.none )

        OnLocationChange location ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    printMediaType MediaTypeChanged



-- VIEW


groupTitle : Model -> Html Msg
groupTitle { route } =
    case route of
        GruppeSide tiltaksGruppe ->
            text (toString tiltaksGruppe.tag)


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


renderTiltakene : Model -> Html Msg
renderTiltakene { route } =
    case route of
        GruppeSide { tiltakene } ->
            ul [] (List.map renderTiltak tiltakene)


renderNav : Model -> List (Html Msg)
renderNav model =
    [ div [] [] ]


view : Model -> Html Msg
view model =
    div []
        [ nav [] (renderNav model)
        , article []
            [ div [] [ groupTitle model ]
            , text model.message
            , input [ onInput Update ] []

            --        , div [] [text ("is print " ++ (toString model.printMediaType))]
            , renderTiltakene model
            ]
        ]
