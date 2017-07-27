module TiltakAndGroupData exposing (..)

import Models exposing (..)
import TiltakStates exposing (TiltakStates)
import Tiltak exposing (Tiltak)
import Tiltak.SykkelparkeringUte as SykkelparkeringUte
import Tiltak.SeparatSykkelveg as SeparatSykkelveg
import Tiltak.LeskurUtenSitteplass as LeskurUtenSitteplass
import Tiltak.SkiltingIBuss as SkiltingIBuss
import Tiltak.LeskurMedSitteplass as LeskurMedSitteplass
import Tiltak.KollektivPrioriteringLyskryss as KollektivPrioriteringLyskryss
import Tiltak.OpphoeyetHoldeplass as OpphoeyetHoldeplass


alleTyper : List Group
alleTyper =
    [ Holdeplasser, Informasjon, StrekningOgFramkommelighet ]


tiltakForGroup : Group -> List Tiltak
tiltakForGroup gruppeType =
    case gruppeType of
        Holdeplasser ->
            [ OpphoeyetHoldeplass.tiltak
            , LeskurMedSitteplass.tiltak

            --            , LeskurUtenSitteplass.tiltak
            --            , SykkelparkeringUte.tiltak
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

        StrekningOgFramkommelighet ->
            [ KollektivPrioriteringLyskryss.tiltak ]


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
    , kollektivPrioriteringLyskryss = KollektivPrioriteringLyskryss.initialState
    , opphoeyetHoldeplass = OpphoeyetHoldeplass.initialState
    }
