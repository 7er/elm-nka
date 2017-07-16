port module Models exposing (..)

import Bootstrap.Navbar as Navbar
import Bootstrap.Modal as Modal
import SykkelparkeringUteTiltak exposing (SykkelparkeringUteTiltakModel)
import SeparatSykkelvegTiltak exposing (SeparatSykkelvegTiltakModel)
import Field exposing (TiltakState)


port generateC3 : String -> Cmd msg


type TiltaksGruppeType
    = Holdeplasser
    | Informasjon


type Page
    = Home
    | GettingStarted
    | GroupPage TiltaksGruppeType
    | NotFound


type alias TiltakStates =
    { sykkelParkeringUteTiltakState : TiltakState SykkelparkeringUteTiltakModel
    , separatSykkelvegTiltakState : TiltakState SeparatSykkelvegTiltakModel
    , leskurUtenSitteplassTiltakState : TiltakState {}
    , skiltingIBussTiltakState : TiltakState {}
    }


type alias Model =
    { page : Page
    , navState : Navbar.State
    , modalState : Modal.State
    , tiltakStates : TiltakStates
    }
