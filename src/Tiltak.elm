module Tiltak exposing (..)

import Models exposing (..)
import Msgs exposing (TiltakObject)
import TiltakGrupper exposing (tiltaksGrupper)


type alias TiltaksGruppe =
    { tag : TiltaksGruppeType
    , tiltakene : List TiltakObject
    }


tiltakene : List TiltakObject
tiltakene =
    let
        updateList group acc =
            acc ++ group.tiltakene
    in
        tiltaksGrupper |> List.foldl updateList []



-- toggleTiltak : TiltakObject -> TiltaksGruppe -> TiltaksGruppe
-- toggleTiltak tiltaket gruppe =
--     let
--         toggleVisible tiltak =
--             case tiltaket == tiltak of
--                 True ->
--                     { tiltak | visible = not tiltak.visible }
--                 False ->
--                     { tiltak | visible = False }
--     in
--         { gruppe | tiltakene = List.map toggleVisible gruppe.tiltakene }


tiltaksGruppePath : TiltaksGruppe -> String
tiltaksGruppePath { tag } =
    tag |> toString |> String.toLower |> (++) "#"


tiltaksGruppeTittel : TiltaksGruppe -> String
tiltaksGruppeTittel { tag } =
    tag |> toString


activeGruppe : TiltaksGruppeType -> Maybe TiltaksGruppe
activeGruppe activeTag =
    let
        filter { tag } =
            tag == activeTag
    in
        tiltaksGrupper |> List.filter filter |> List.head


gruppeFromHash : String -> Maybe TiltaksGruppeType
gruppeFromHash hash =
    let
        filter gruppe =
            hash == tiltaksGruppePath gruppe
    in
        case List.head (List.filter filter tiltaksGrupper) of
            Just gruppe ->
                Just gruppe.tag

            Nothing ->
                Nothing
