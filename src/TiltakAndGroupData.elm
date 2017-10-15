module TiltakAndGroupData exposing (..)

import Models exposing (..)
import TiltakStates exposing (TiltakStates)
import Tiltak exposing (Tiltak)
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
            , SimpleTiltak.createTiltak
                { stateMap =
                    \func tiltakStates ->
                        { tiltakStates
                            | leskurMedSitteplass = func tiltakStates.leskurMedSitteplass
                        }
                , getter = .leskurMedSitteplass
                , nytteMultiplikator = verdisettinger.leskurPaaBussholdeplassenMedSitteplass
                , levetid = 12
                , title = "Pakke: Leskur og sitteplass på holdeplass"
                }
            , SimpleTiltak.createTiltak
                { stateMap =
                    \func tiltakStates ->
                        { tiltakStates
                            | leskurUtenSitteplass = func tiltakStates.leskurUtenSitteplass
                        }
                , getter =
                    .leskurUtenSitteplass
                , nytteMultiplikator =
                    verdisettinger.leskurPaaBussholdeplassenUtenSitteplass
                , levetid =
                    12
                , title =
                    "Leskur uten sitteplass"
                }
            , SimpleTiltak.createTiltak
                { stateMap =
                    \func tiltakStates ->
                        { tiltakStates
                            | sitteplassPaaHpl = func tiltakStates.sitteplassPaaHpl
                        }
                , getter = .sitteplassPaaHpl
                , nytteMultiplikator = verdisettinger.sitteplassPaaHpl
                , levetid = 12
                , title = "Sitteplass på holdeplass"
                }
            ]

        Informasjon ->
            [ SimpleTiltak.createTiltak
                { stateMap =
                    \func tiltakStates ->
                        { tiltakStates
                            | skiltingIBuss = func tiltakStates.skiltingIBuss
                        }
                , getter = .skiltingIBuss
                , nytteMultiplikator = verdisettinger.skiltingIBuss
                , levetid = 7
                , title = "Elektronisk skilting i bussen av neste holdeplass"
                }
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
    { leskurUtenSitteplass = SimpleTiltak.initialState
    , skiltingIBuss = SimpleTiltak.initialState
    , leskurMedSitteplass = SimpleTiltak.initialState
    , kollektivPrioriteringLyskryss = KollektivPrioriteringLyskryss.initialState
    , opphoeyetHoldeplass = OpphoeyetHoldeplass.initialState
    , belysning = SimpleTiltak.initialState
    , sitteplassPaaHpl = SimpleTiltak.initialState
    }
