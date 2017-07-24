port module Models exposing (..)

import Bootstrap.Navbar as Navbar
import Bootstrap.Modal as Modal
import Bootstrap.Accordion as Accordion
import TiltakStates exposing (TiltakStates)


port generateC3 : String -> Cmd msg


type Group
    = Holdeplasser
    | Informasjon
    | StrekningOgFramkommelighet


type Page
    = Home
    | GettingStarted
    | GroupPage Group
    | NotFound


type alias Model =
    { page : Page
    , navState : Navbar.State
    , modalState : Modal.State
    , accordionState : Accordion.State
    , tiltakStates : TiltakStates
    }
