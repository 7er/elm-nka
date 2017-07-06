port module ModelAndMsg exposing (..)

import Navigation exposing (Location)
import Bootstrap.Navbar as Navbar
import Bootstrap.Modal as Modal


port generateC3 : String -> Cmd msg


type alias VariableName =
    String


type alias SykkelparkeringUteFormState =
    { tripsPerYear : Maybe Int
    , yearlyMaintenance : Maybe Int
    , installationCost : Maybe Int
    , submitted : Bool
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
