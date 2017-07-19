module TiltakAndGroupData exposing (..)

import Models exposing (..)
import TiltakStates exposing (TiltakStates)
import TiltakComponents.SykkelparkeringUte as SykkelparkeringUte
import TiltakComponents.SeparatSykkelveg as SeparatSykkelveg
import TiltakComponents.LeskurUtenSitteplass as LeskurUtenSitteplass
import TiltakComponents.SkiltingIBuss as SkiltingIBuss


alleTyper : List TiltaksGruppeType
alleTyper =
    [ Holdeplasser, Informasjon ]


tiltakForGroup : TiltaksGruppeType -> List Tiltak
tiltakForGroup gruppeType =
    case gruppeType of
        Holdeplasser ->
            [ SykkelparkeringUte.tiltak
            , LeskurUtenSitteplass.tiltak

            {-
               , { name = "Sitteplass på hpl"
                 , page = \model -> [ text "Sitteplass side" ]
                 , toggleVisible = \model -> model
                 }
            -}
            ]

        Informasjon ->
            [ SkiltingIBuss.tiltak

            --            , TiltakObject "Hpl. opprop" (\model -> [ text "Hpl. opprop side" ]) (\model -> model)
            , SeparatSykkelveg.tiltak
            ]


alleTiltak : List Tiltak
alleTiltak =
    alleTyper |> List.concatMap tiltakForGroup


initialTiltakStates : TiltakStates
initialTiltakStates =
    { sykkelParkeringUteTiltakState = SykkelparkeringUte.initialState
    , separatSykkelvegTiltakState = SeparatSykkelveg.initialState
    , leskurUtenSitteplassTiltakState = {}
    , skiltingIBussTiltakState = {}
    }
