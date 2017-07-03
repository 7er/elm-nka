port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (onInput, onClick)


port printMediaType : (Bool -> msg) -> Sub msg



-- MODEL


type TiltaksGruppeType
    = Holdeplasser
    | Informasjon


type alias Tiltak =
    { name : String
    , data : String
    , visible : Bool
    }


type alias TiltaksGruppe =
    { tag : TiltaksGruppeType
    , tiltakene : List Tiltak
    }


type Route
    = GruppeSide TiltaksGruppe


type alias Model =
    { message : String
    , printMediaType : Bool
    , tiltaksGrupper : List TiltaksGruppe
    , route : Route
    }


createTiltak : String -> String -> Tiltak
createTiltak name data =
    { name = name
    , data = data
    , visible = False
    }


init : ( Model, Cmd Msg )
init =
    ( { message = "Hello"
      , printMediaType = False
      , tiltaksGrupper = []
      , route =
            GruppeSide
                { tag = Holdeplasser
                , tiltakene =
                    [ createTiltak "Sykkelparkering" "Foobar"
                    , createTiltak "Leskur u sitteplass" "Zppt "
                    , createTiltak "Sitteplass på hpl" "Syver "
                    ]
                }
      }
    , Cmd.none
    )



-- MESSAGES


type Msg
    = NoOp
    | Update String
    | MediaTypeChanged Bool
    | ToggleVisible Tiltak



-- VIEW


groupTitle : Model -> Html Msg
groupTitle { route } =
    case route of
        GruppeSide tiltaksGruppe ->
            text (toString tiltaksGruppe.tag)


renderTiltak : Tiltak -> Html Msg
renderTiltak tiltak =
    let
        baseContent =
            [ h3 [] [ a [ href "#", onClick (ToggleVisible tiltak) ] [ text tiltak.name ] ] ]

        content =
            case tiltak.visible of
                True ->
                    baseContent ++ [ div [] [ text tiltak.data ] ]

                False ->
                    baseContent
    in
        li [] content


renderTiltakene : Model -> Html Msg
renderTiltakene { route } =
    case route of
        GruppeSide { tiltakene } ->
            ul [] (List.map renderTiltak tiltakene)


view : Model -> Html Msg
view model =
    div []
        [ div [] [ groupTitle model ]
        , text model.message
        , input [ onInput Update ] []

        --        , div [] [text ("is print " ++ (toString model.printMediaType))]
        , renderTiltakene model
        ]



-- UPDATE


toggleTiltak : Model -> Tiltak -> Model
toggleTiltak model tiltaket =
    let
        toggleVisible tiltak =
            case tiltaket == tiltak of
                True ->
                    { tiltak | visible = not tiltak.visible }

                False ->
                    { tiltak | visible = False }

        tiltaksGruppe =
            case model.route of
                GruppeSide tiltaksGruppa ->
                    { tiltaksGruppa | tiltakene = List.map toggleVisible tiltaksGruppa.tiltakene }
    in
        { model | route = GruppeSide tiltaksGruppe }


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    printMediaType MediaTypeChanged



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
