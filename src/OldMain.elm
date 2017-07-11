port module Main exposing (..)

import Navigation exposing (Location)
import Models exposing (..)
import Msgs exposing (..)
import Views exposing (view)


-- MAIN


main : Program Never Model Msg
main =
    Navigation.program
        OnLocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- UPDATE


init : Location -> ( Model, Cmd Msg )
init location =
    let
        model =
            { tiltaksGrupper = tiltaksGrupper
            , route = routeFromLocation location
            }
    in
        ( model
        , case model.route of
            Root ->
                case List.head tiltaksGrupper of
                    Just gruppe ->
                        Navigation.modifyUrl (location.href ++ tiltaksGruppePath gruppe)

                    _ ->
                        Cmd.none

            _ ->
                Cmd.none
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "update: " msg of
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
    Sub.none
