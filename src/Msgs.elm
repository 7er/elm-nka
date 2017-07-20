module Msgs exposing (..)

import Navigation exposing (Location)
import Bootstrap.Navbar as Navbar
import Bootstrap.Modal as Modal
import Bootstrap.Accordion as Accordion
import Models exposing (..)


type Msg
    = UrlChange Location
    | NavMsg Navbar.State
    | ModalMsg Modal.State
    | AccordionMsg Accordion.State
    | UpdateField Field String
