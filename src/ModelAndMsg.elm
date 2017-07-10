port module ModelAndMsg exposing (..)

import Navigation exposing (Location)
import Bootstrap.Navbar as Navbar
import Bootstrap.Modal as Modal
import SykkelparkeringUteTiltak exposing (SykkelparkeringUteTiltakModel)
import SeparatSykkelvegTiltak exposing (SeparatSykkelvegTiltakModel)
import Field exposing (..)


port generateC3 : String -> Cmd msg


type Page
    = Home
    | GettingStarted
    | SykkelparkeringUte
    | SeparatSykkelveg
    | NotFound


type alias Model =
    { page : Page
    , navState : Navbar.State
    , modalState : Modal.State
    , sykkelParkeringUteFormState : FormState SykkelparkeringUteTiltakModel
    , separatSykkelvegFormState : FormState SeparatSykkelvegTiltakModel
    }


type alias FieldValue =
    String


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
