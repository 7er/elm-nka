port module Main exposing (..)

import Navigation exposing (Location)
import Models exposing (..)
import Msgs exposing (..)
import Views exposing (view)


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
            ( { model | tiltaksGrupper = List.map (toggleTiltak tiltak) model.tiltaksGrupper }
            , Cmd.none
            )

        UpdateData tiltak newData ->
            ( { model | tiltaksGrupper = List.map (updateData tiltak newData) model.tiltaksGrupper }
            , Cmd.none
            )

        OnLocationChange location ->
            ( { model | route = routeFromLocation location }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    printMediaType MediaTypeChanged
