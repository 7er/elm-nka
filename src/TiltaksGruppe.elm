module TiltaksGruppe exposing (..)

import Models exposing (..)
import Msgs exposing (TiltakWidget)
import PageSykkelparkeringUte
import PageSeparatSykkelveg
import PageLeskurUtenSitteplass
import PageSkiltingIBuss


type alias TiltaksGruppe =
    { tag : TiltaksGruppeType
    , tiltakene : List TiltakWidget
    }


tiltaksGrupper : List TiltaksGruppe
tiltaksGrupper =
    [ { tag = Holdeplasser
      , tiltakene =
            [ PageSykkelparkeringUte.tiltakWidget
            , PageLeskurUtenSitteplass.tiltakWidget

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
            [ PageSkiltingIBuss.tiltakWidget

            --            , TiltakWidget "Hpl. opprop" (\model -> [ text "Hpl. opprop side" ]) (\model -> model)
            , PageSeparatSykkelveg.tiltakWidget
            ]
      }
    ]



-- toggleTiltak : TiltakWidget -> TiltaksGruppe -> TiltaksGruppe
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
