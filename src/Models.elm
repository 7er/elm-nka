port module Models exposing (..)

import Bootstrap.Accordion as Accordion
import TiltakStates exposing (TiltakStates)


type Group
    = Holdeplasser
    | Informasjon
    | Trygghet
    | Kjoeremateriell
    | StrekningOgFramkommelighet
    | Tilgjengelighet


type Page
    = Home
    | GroupPage Group
    | NotFound


type alias Model =
    { page : Page
    , accordionState : Accordion.State
    , tiltakStates : TiltakStates
    , chartIds : List String
    }
