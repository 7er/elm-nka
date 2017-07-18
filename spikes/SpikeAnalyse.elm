module SpikeAnalyse exposing (..)

import Html exposing (Html, div, text, h2)
import Html.Attributes exposing (for)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import TiltakStates exposing (TiltakStates)
import Types exposing (Tiltak, Field)
import Tiltak.Foo
import Tiltak.Bar


type alias Model =
    { tiltakStates : TiltakStates
    }


type Msg
    = UpdateField Tiltak Field String


updateTiltakStateFromField : Tiltak -> Field -> String -> TiltakStates -> TiltakStates
updateTiltakStateFromField tiltak field stringValue tiltakStates =
    field.updateTiltakState stringValue tiltakStates


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateField tiltak field stringValue ->
            { model | tiltakStates = updateTiltakStateFromField tiltak field stringValue model.tiltakStates }


init : Model
init =
    { tiltakStates = { fooTiltak = Tiltak.Foo.initialState, barTiltak = { x = Just 1, y = Just 2 } } }


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = init
        , view = view
        , update = update
        }


tiltakene : List Tiltak
tiltakene =
    [ Tiltak.Foo.tiltak
    , Tiltak.Bar.tiltak
    ]


form : Tiltak -> Model -> Html Msg
form tiltak model =
    let
        fieldView ({ name, title, placeholder } as field) =
            Form.group []
                [ Form.label [ for name ] [ text title ]
                , Input.number
                    [ Input.id name
                    , Input.placeholder placeholder
                    , Input.onInput (UpdateField tiltak field)
                    ]
                ]
    in
        Form.form [] (tiltak.fields |> List.map fieldView)


page : Tiltak -> Model -> List (Html Msg)
page tiltak model =
    [ h2 [] [ text tiltak.title ]
    , form tiltak model
    , div [] [ text ("Tiltaket som kalkulerer " ++ (toString (tiltak.calculation model.tiltakStates))) ]
    ]


view : Model -> Html Msg
view model =
    let
        tiltakDiv tiltak =
            div [] <| page tiltak model
    in
        Grid.container []
            [ CDN.stylesheet
            , div [] (tiltakene |> List.map tiltakDiv)
            ]
