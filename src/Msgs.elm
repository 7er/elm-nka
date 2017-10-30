module Msgs exposing (..)

import Navigation exposing (Location)
import Bootstrap.Accordion as Accordion
import Tiltak exposing (Tiltak)
import Field exposing (Field)


type Msg
    = UrlChange Location
    | AccordionMsg Accordion.State
    | UpdateField Tiltak Field String
    | UpdateBooleanField Field Bool
    | ChartsChanged (List String)
