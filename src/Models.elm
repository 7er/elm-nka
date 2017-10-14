port module Models exposing (..)

import Bootstrap.Navbar as Navbar
import Bootstrap.Modal as Modal
import Bootstrap.Accordion as Accordion
import TiltakStates exposing (TiltakStates)


type Group
    = Holdeplasser
    | Informasjon
    | Trygghet
    | Kjoeremateriell
    | StrekningOgFramkommelighet


type Page
    = Home
    | GroupPage Group
    | NotFound


type alias Model =
    { page : Page
    , navState : Navbar.State
    , modalState : Modal.State
    , accordionState : Accordion.State
    , tiltakStates : TiltakStates
    , chartIds : List String
    }
