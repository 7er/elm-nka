port module ModelAndMsg exposing (..)

import Navigation exposing (Location)
import Bootstrap.Navbar as Navbar
import Bootstrap.Modal as Modal


port generateC3 : String -> Cmd msg


type alias VariableName =
    String


type alias SykkelparkeringUteFormState a =
    { a
        | tripsPerYear : Maybe Int
        , yearlyMaintenance : Maybe Float
        , installationCost : Maybe Float
        , submitted : Bool
    }


type Page
    = Home
    | GettingStarted
    | Modules
    | SykkelparkeringUte
    | NotFound


type alias Model =
    SykkelparkeringUteFormState
        { page : Page
        , navState : Navbar.State
        , modalState : Modal.State
        }


type Msg
    = UrlChange Location
    | NavMsg Navbar.State
    | ModalMsg Modal.State
    | SykkelparkeringUteSubmit
    | SykkelparkeringUteForm VariableName String
