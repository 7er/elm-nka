module ModelAndMsg exposing (..)

import Navigation exposing (Location)
import Bootstrap.Navbar as Navbar
import Bootstrap.Modal as Modal


type alias VariableName =
    String


type alias SykkelparkeringUteFormState =
    { tripsPerYear : Maybe Int
    }


type Page
    = Home
    | GettingStarted
    | Modules
    | SykkelparkeringUte
    | NotFound


type alias Model =
    { page : Page
    , navState : Navbar.State
    , modalState : Modal.State
    , sykkelParkeringUteFormState : SykkelparkeringUteFormState
    }


type Msg
    = UrlChange Location
    | NavMsg Navbar.State
    | ModalMsg Modal.State
    | SykkelparkeringUteSubmit
    | SykkelparkeringUteForm VariableName String
