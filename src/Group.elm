module Group exposing (..)

import Models exposing (TiltaksGruppeType, Tiltak)
import TiltakAndGroupData


tiltaksGruppePath : TiltaksGruppeType -> String
tiltaksGruppePath tag =
    tag |> toString |> String.toLower |> (++) "#"


tiltaksGruppeTittel : TiltaksGruppeType -> String
tiltaksGruppeTittel tag =
    tag |> toString


tiltakForGroup =
    TiltakAndGroupData.tiltakForGroup


alleTyper =
    TiltakAndGroupData.alleTyper


gruppeFromHash : String -> Maybe TiltaksGruppeType
gruppeFromHash hash =
    let
        filter gruppe =
            hash == tiltaksGruppePath gruppe
    in
        TiltakAndGroupData.alleTyper |> List.filter filter |> List.head
