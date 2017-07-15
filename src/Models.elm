port module Models exposing (..)

import Dict exposing (Dict)
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


type alias Model =
    { page : Page
    , navState : Navbar.State
    , modalState : Modal.State
    , sykkelParkeringUteTiltakState : TiltakState SykkelparkeringUteTiltakModel
    , separatSykkelvegTiltakState : TiltakState SeparatSykkelvegTiltakModel

    --    , leskurUtenSitteplassTiltakState : TiltakState {}
    , skiltingIBussTiltakState : TiltakState {}
    , tiltakComponentState : Dict String TiltakComponentState
    }


type TiltakComponentState
    = SykkelparkeringUteState (TiltakState SykkelparkeringUteTiltakModel)
    | SeparatSykkelvegState (TiltakState SeparatSykkelvegTiltakModel)
    | LeskurUtenSitteplassState (TiltakState {})
    | SkiltingIBussState (TiltakState {})


type alias NameToComponentStates =
    Dict String TiltakComponentState


tiltakStateFor : String -> NameToComponentStates -> Maybe TiltakComponentState
tiltakStateFor name tiltakComponentState =
    tiltakComponentState |> Dict.get name


toggleVisibleFor :
    String
    -> NameToComponentStates
    -> NameToComponentStates
toggleVisibleFor name model =
    Debug.crash "TODO"
