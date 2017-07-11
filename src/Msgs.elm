module Msgs exposing (..)

import Navigation exposing (Location)
import Models exposing (..)


type Flesk
    = ToggleVisible Tiltak
    | UpdateData Tiltak String
    | OnLocationChange Location
