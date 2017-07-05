module Msgs exposing (..)

import Navigation exposing (Location)
import Models exposing (..)


type Msg
    = ToggleVisible Tiltak
    | UpdateData Tiltak String
    | OnLocationChange Location
