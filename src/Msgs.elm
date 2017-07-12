module Msgs exposing (..)

import Navigation exposing (Location)
import Bootstrap.Navbar as Navbar
import Bootstrap.Modal as Modal
import Models exposing (..)
import Field exposing (..)
import Html exposing (Html)


type alias UpdateFunc =
    FieldValue -> Model -> Model


type alias SubmitFunc =
    Model -> ( Model, Cmd Msg )


type Msg
    = UrlChange Location
    | NavMsg Navbar.State
    | ModalMsg Modal.State
    | FieldUpdate UpdateFunc String
    | FormSubmit SubmitFunc
    | ToggleVisible TiltakWidget


type alias TiltakWidget =
    { name : String
    , page : Model -> List (Html Msg)
    , toggleVisible : Model -> Model
    , isVisible : Model -> Bool
    }
