module Group exposing (..)

import Models exposing (Group)
import TiltakAndGroupData


tiltaksGruppePath : Group -> String
tiltaksGruppePath tag =
    tag |> toString |> String.toLower |> (++) "#"


tiltaksGruppeTittel : Group -> String
tiltaksGruppeTittel tag =
    tag |> toString


tiltakForGroup =
    TiltakAndGroupData.tiltakForGroup


alleTyper =
    TiltakAndGroupData.alleTyper


gruppeFromHash : String -> Maybe Group
gruppeFromHash hash =
    let
        filter gruppe =
            hash == tiltaksGruppePath gruppe
    in
        TiltakAndGroupData.alleTyper |> List.filter filter |> List.head
