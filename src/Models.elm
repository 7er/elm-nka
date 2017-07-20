port module Models exposing (..)

import Bootstrap.Navbar as Navbar
import Bootstrap.Modal as Modal
import Bootstrap.Accordion as Accordion
import TiltakStates exposing (TiltakStates)


port generateC3 : String -> Cmd msg


type Group
    = Holdeplasser
    | Informasjon


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


type alias Field =
    { name : String
    , title : String
    , placeholder : String
    , updateTiltakState : String -> TiltakStates -> TiltakStates
    , stringValueFromState : TiltakStates -> String
    }


type alias Tiltak =
    { brukerNytte : TiltakStates -> Maybe Float
    , kostUtenSkyggepris : TiltakStates -> Maybe Float
    , title : String
    , fields : List Field
    }
