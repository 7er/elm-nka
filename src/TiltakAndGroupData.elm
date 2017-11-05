module TiltakAndGroupData exposing (..)

import Models exposing (..)
import TiltakStates exposing (TiltakStates)
import Tiltak exposing (Tiltak)
import Tiltak.KollektivPrioriteringLyskryss as KollektivPrioriteringLyskryss
import Tiltak.KollektivPrioriteringSkilting as KollektivPrioriteringSkilting
import Tiltak.OpphoeyetHoldeplass as OpphoeyetHoldeplass
import Tiltak.HplOpprop as HplOpprop
import Tiltak.Bussrenhold as Bussrenhold
import Tiltak.Laventrebuss as Laventrebuss
import Tiltak.Kantsteinstopp as Kantsteinstopp
import SimpleTiltak
import SuperSimpleTiltak
import GeneralForutsetninger exposing (verdisettinger)


alleTyper : List Group
alleTyper =
    [ Holdeplasser
    , Informasjon
    , Trygghet
    , Kjoeremateriell
    , StrekningOgFramkommelighet
    , Tilgjengelighet
    ]



-- TODO: organize simple tiltak in a Collection with tiltak and
-- initial state paired together


leskurMedSitteplassTiltak : Tiltak
leskurMedSitteplassTiltak =
    SimpleTiltak.createTiltak
        { stateMap =
            \func tiltakStates ->
                { tiltakStates
                    | leskurMedSitteplass = func tiltakStates.leskurMedSitteplass
                }
        , getter = .leskurMedSitteplass
        , nytteMultiplikator = verdisettinger.leskurPaaBussholdeplassenMedSitteplass
        , levetid = 12
        , title = "Leskur med sitteplass"
        }


sitteplassPaaHplTiltak =
    SimpleTiltak.createTiltak
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


renholdPaaHplTiltak =
    SuperSimpleTiltak.createTiltak
        { stateMap =
            \func tiltakStates ->
                { tiltakStates
                    | renholdPaaHpl = func tiltakStates.renholdPaaHpl
                }
        , getter = .renholdPaaHpl
        , nytteMultiplikator = verdisettinger.renholdPaaHpl
        , title = "Renhold på holdeplass"
        }


fjerningAvIsSnoePaaHplTiltak =
    SuperSimpleTiltak.createTiltak
        { stateMap =
            \func tiltakStates ->
                { tiltakStates
                    | fjerningAvIsSnoePaaHpl = func tiltakStates.fjerningAvIsSnoePaaHpl
                }
        , getter = .fjerningAvIsSnoePaaHpl
        , nytteMultiplikator = verdisettinger.fjerningAvIsSnoePaaHpl
        , title = "Økt fjerning av is og snø på holdeplass"
        }


skiltingIBussTiltak =
    SimpleTiltak.createTiltak
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


pakkeSkiltOgOppropBussTiltak =
    SimpleTiltak.createTiltak
        { stateMap =
            \func tiltakStates ->
                { tiltakStates
                    | pakkeSkiltOgOppropBuss = func tiltakStates.pakkeSkiltOgOppropBuss
                }
        , getter = .pakkeSkiltOgOppropBuss
        , nytteMultiplikator = verdisettinger.pakkeSkiltOgOppropBuss
        , levetid = 7
        , title =
            "Elektronisk skilting og opprop i bussen av neste holdeplass"
        }


avviksinformasjonHoeyttalerTiltak =
    SimpleTiltak.createTiltak
        { stateMap =
            \func tiltakStates ->
                { tiltakStates
                    | avviksinformasjonHoeyttaler = func tiltakStates.avviksinformasjonHoeyttaler
                }
        , getter = .avviksinformasjonHoeyttaler
        , nytteMultiplikator = verdisettinger.avviksinformasjonHoeyttaler
        , levetid = 20
        , title =
            "Informasjon over høyttaler på holdeplass om avvik"
        }


belysningTiltak =
    SimpleTiltak.createTiltak
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


tiltakForGroup : Group -> List Tiltak
tiltakForGroup gruppeType =
    case gruppeType of
        Holdeplasser ->
            [ -- Sykkelparkering.tiltak
              SimpleTiltak.createTiltak
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
            , sitteplassPaaHplTiltak
            , leskurMedSitteplassTiltak
            , renholdPaaHplTiltak
            , fjerningAvIsSnoePaaHplTiltak
            , OpphoeyetHoldeplass.tiltak

            {--

Dette er mer komplekst, har 2 sett med forutsetninger
            , SimpleTiltak.createTiltak
                { stateMap =
                    \func tiltakStates ->
                        { tiltakStates
                            | lokalkartPaaHpl = func tiltakStates.lokalkartPaaHpl
                        }
                , getter = .lokalkartPaaHpl
                , nytteMultiplikator = verdisettinger.lokalkartPaaHpl
                , levetid = 10
                , title = "Lokalkart på holdeplass"
                }

            , SimpleTiltak.createTiltak
                { stateMap =
                    \func tiltakStates ->
                        { tiltakStates
                            | rutekartPaaHpl = func tiltakStates.rutekartPaaHpl
                        }
                , getter = .rutekartPaaHpl
                , nytteMultiplikator = verdisettinger.rutekartPaaHpl
                , levetid = 1
                , title = "Rutekart på holdeplass"
                }
--}
            ]

        Informasjon ->
            [ skiltingIBussTiltak
            , HplOpprop.tiltak
            , pakkeSkiltOgOppropBussTiltak
            , SimpleTiltak.createTiltak
                { stateMap =
                    \func tiltakStates ->
                        { tiltakStates
                            | destinasjonsSkiltPaaBuss = func tiltakStates.destinasjonsSkiltPaaBuss
                        }
                , getter = .destinasjonsSkiltPaaBuss
                , nytteMultiplikator = verdisettinger.destinasjonsSkiltPaaBuss
                , levetid = 10
                , title =
                    "Destinasjonsskilt bak og på siden av bussen"
                }
            , avviksinformasjonHoeyttalerTiltak
            ]

        Trygghet ->
            [ belysningTiltak
            , SimpleTiltak.createTiltak
                { stateMap =
                    \func tiltakStates ->
                        { tiltakStates
                            | alarmsystemPaaHpl = func tiltakStates.alarmsystemPaaHpl
                        }
                , getter = .alarmsystemPaaHpl
                , nytteMultiplikator = verdisettinger.alarmsystemPaaHpl
                , levetid = 20
                , title = "Alarmsystem / Nødtelefon på holdeplass"
                }
            , SuperSimpleTiltak.createTiltak
                { stateMap =
                    \func tiltakStates ->
                        { tiltakStates
                            | vektere = func tiltakStates.vektere
                        }
                , getter = .vektere
                , nytteMultiplikator = verdisettinger.vektere
                , title = "Vektere på holdeplass"
                }
            ]

        Kjoeremateriell ->
            [ Bussrenhold.tiltak
            , Laventrebuss.tiltak
            ]

        StrekningOgFramkommelighet ->
            [ Kantsteinstopp.tiltak
            , KollektivPrioriteringLyskryss.tiltak
            , KollektivPrioriteringSkilting.tiltak
            ]

        Tilgjengelighet ->
            [ OpphoeyetHoldeplass.tiltak
            , leskurMedSitteplassTiltak
            , sitteplassPaaHplTiltak
            , renholdPaaHplTiltak
            , fjerningAvIsSnoePaaHplTiltak
            , skiltingIBussTiltak
            , HplOpprop.tiltak
            , pakkeSkiltOgOppropBussTiltak
            , avviksinformasjonHoeyttalerTiltak
            , belysningTiltak
            , Laventrebuss.tiltak
            ]


initialTiltakStates : TiltakStates
initialTiltakStates =
    { leskurUtenSitteplass = SimpleTiltak.initialState
    , skiltingIBuss = SimpleTiltak.initialState
    , leskurMedSitteplass = SimpleTiltak.initialState
    , kollektivPrioriteringLyskryss = KollektivPrioriteringLyskryss.initialState
    , opphoeyetHoldeplass = OpphoeyetHoldeplass.initialState
    , belysning = SimpleTiltak.initialState
    , sitteplassPaaHpl = SimpleTiltak.initialState
    , lokalkartPaaHpl = SimpleTiltak.initialState
    , rutekartPaaHpl = SimpleTiltak.initialState
    , pakkeSkiltOgOppropBuss = SimpleTiltak.initialState
    , destinasjonsSkiltPaaBuss = SimpleTiltak.initialState
    , avviksinformasjonHoeyttaler = SimpleTiltak.initialState
    , alarmsystemPaaHpl = SimpleTiltak.initialState
    , renholdPaaHpl = SuperSimpleTiltak.initialState
    , fjerningAvIsSnoePaaHpl = SuperSimpleTiltak.initialState
    , vektere = SuperSimpleTiltak.initialState
    , hplOpprop = HplOpprop.initialState
    , kollektivPrioriteringSkilting = KollektivPrioriteringSkilting.initialState
    , bussrenhold = Bussrenhold.initialState
    , laventrebuss = Laventrebuss.initialState
    , kantsteinstopp = Kantsteinstopp.initialState
    }
