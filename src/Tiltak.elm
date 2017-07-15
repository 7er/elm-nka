module Tiltak exposing (..)

import Dict exposing (Dict(..))
import Models exposing (..)
import Msgs exposing (TiltakObject)
import TiltakComponents.SykkelparkeringUte as SykkelparkeringUte
import TiltakComponents.SeparatSykkelveg as SeparatSykkelveg
import TiltakComponents.LeskurUtenSitteplass as LeskurUtenSitteplass
import TiltakComponents.SkiltingIBuss as SkiltingIBuss


type alias TiltaksGruppe =
    { tag : TiltaksGruppeType
    , tiltakene : List TiltakObject
    }


initialTiltakComponentState : NameToComponentStates
initialTiltakComponentState =
    let
        updateDict : TiltakObject -> NameToComponentStates -> NameToComponentStates
        updateDict tiltak dict =
            dict |> Dict.insert tiltak.name tiltak.initialState
    in
        tiltakene |> List.foldl updateDict Dict.empty


tiltakene : List TiltakObject
tiltakene =
    let
        updateList group acc =
            acc ++ group.tiltakene
    in
        tiltaksGrupper |> List.foldl updateList []


tiltaksGrupper : List TiltaksGruppe
tiltaksGrupper =
    [ { tag = Holdeplasser
      , tiltakene =
            [ SykkelparkeringUte.tiltakObject
            , LeskurUtenSitteplass.tiltakObject

            {-
               , { name = "Sitteplass på hpl"
                 , page = \model -> [ text "Sitteplass side" ]
                 , toggleVisible = \model -> model
                 }
            -}
            ]
      }
    , { tag = Informasjon
      , tiltakene =
            [ SkiltingIBuss.tiltakObject

            --            , TiltakObject "Hpl. opprop" (\model -> [ text "Hpl. opprop side" ]) (\model -> model)
            , SeparatSykkelveg.tiltakObject
            ]
      }
    ]



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
