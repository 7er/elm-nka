module TiltakAndGroupData exposing (..)

import Models exposing (..)
import TiltakStates exposing (TiltakStates)
import Tiltak exposing (Tiltak)
import Tiltak.SykkelparkeringUte as SykkelparkeringUte
import Tiltak.SeparatSykkelveg as SeparatSykkelveg
import Tiltak.LeskurUtenSitteplass as LeskurUtenSitteplass
import Tiltak.SkiltingIBuss as SkiltingIBuss
import Tiltak.LeskurMedSitteplass as LeskurMedSitteplass


alleTyper : List Group
alleTyper =
    [ Holdeplasser, Informasjon ]


tiltakForGroup : Group -> List Tiltak
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
    { sykkelParkeringUte = SykkelparkeringUte.initialState
    , separatSykkelveg = SeparatSykkelveg.initialState
    , leskurUtenSitteplass = LeskurMedSitteplass.initialState
    , skiltingIBuss = LeskurMedSitteplass.initialState
    , leskurMedSitteplass = LeskurMedSitteplass.initialState
    }
