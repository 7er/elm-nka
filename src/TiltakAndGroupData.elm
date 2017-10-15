module TiltakAndGroupData exposing (..)

import Models exposing (..)
import TiltakStates exposing (TiltakStates)
import Tiltak exposing (Tiltak)


-- import Tiltak.SykkelparkeringUte as SykkelparkeringUte
-- import Tiltak.SeparatSykkelveg as SeparatSykkelveg

import Tiltak.SkiltingIBuss as SkiltingIBuss
import Tiltak.LeskurMedSitteplass as LeskurMedSitteplass
import Tiltak.LeskurUtenSitteplass as LeskurUtenSitteplass
import Tiltak.KollektivPrioriteringLyskryss as KollektivPrioriteringLyskryss
import Tiltak.OpphoeyetHoldeplass as OpphoeyetHoldeplass
import SimpleTiltak
import GeneralForutsetninger exposing (verdisettinger)


-- TODO: organize simple tiltak in a Collection with tiltak and
-- initial state paired together


alleTyper : List Group
alleTyper =
    [ Holdeplasser
    , Informasjon
    , Trygghet
    , Kjoeremateriell
    , StrekningOgFramkommelighet
    ]


tiltakForGroup : Group -> List Tiltak
tiltakForGroup gruppeType =
    case gruppeType of
        Holdeplasser ->
            [ OpphoeyetHoldeplass.tiltak
            , LeskurMedSitteplass.tiltak
            , LeskurUtenSitteplass.tiltak

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
            -- , SeparatSykkelveg.tiltak
            ]

        Trygghet ->
            [ SimpleTiltak.createTiltak
                { stateMap =
                    \func tiltakStates ->
                        { tiltakStates
                            | belysning = func tiltakStates.belysning
                        }
                , getter = .belysning
                , nytteMultiplikator = verdisettinger.belysning
                , levetid = 20
                , title = "Belysning på holdeplass"
                }
            ]

        Kjoeremateriell ->
            []

        StrekningOgFramkommelighet ->
            [ KollektivPrioriteringLyskryss.tiltak ]


alleTiltak : List Tiltak
alleTiltak =
    alleTyper |> List.concatMap tiltakForGroup


initialTiltakStates : TiltakStates
initialTiltakStates =
    { leskurUtenSitteplass = LeskurMedSitteplass.initialState
    , skiltingIBuss = LeskurMedSitteplass.initialState
    , leskurMedSitteplass = LeskurMedSitteplass.initialState
    , kollektivPrioriteringLyskryss = KollektivPrioriteringLyskryss.initialState
    , opphoeyetHoldeplass = OpphoeyetHoldeplass.initialState
    , belysning = SimpleTiltak.initialState
    }
