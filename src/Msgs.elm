module Msgs exposing (..)

import Navigation exposing (Location)
import Bootstrap.Navbar as Navbar
import Bootstrap.Modal as Modal
import Bootstrap.Accordion as Accordion
import Tiltak exposing (Tiltak)
import Field exposing (Field)


type Msg
    = UrlChange Location
    | NavMsg Navbar.State
    | ModalMsg Modal.State
    | AccordionMsg Accordion.State
    | UpdateField Tiltak Field String
    | UpdateBooleanField Field Bool
