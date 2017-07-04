module Msgs exposing (..)

import Navigation exposing (Location)
import Models exposing (..)


type Msg
    = NoOp
    | Update String
    | MediaTypeChanged Bool
    | ToggleVisible Tiltak
    | UpdateData Tiltak String
    | OnLocationChange Location
