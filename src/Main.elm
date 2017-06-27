port module Main exposing (..)

import Html exposing (Html, div, input, program, text)
import Html.Events exposing (onInput)


port printMediaType : (Bool -> msg) -> Sub msg



-- MODEL


type alias Model =
    { message : String
    , printMediaType : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { message = "Hello", printMediaType = False }, Cmd.none )



-- MESSAGES


type Msg
    = NoOp
    | Update String
    | MediaTypeChanged Bool



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text model.message
        , input [ onInput Update ] []
        --        , div [] [text ("is print " ++ (toString model.printMediaType))]
        ]




-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "update: " msg of
        NoOp ->
            ( model, Cmd.none )

        Update inputString ->
            ( {model | message = inputString}, Cmd.none )

        MediaTypeChanged isPrintType ->
            ( { model | printMediaType = isPrintType }, Cmd.none )



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
