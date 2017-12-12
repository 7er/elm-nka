module TiltakAndGroupData exposing (..)

import Focus
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
        { focus =
            Focus.create .leskurMedSitteplass
                (\func tiltakStates ->
                    { tiltakStates
                        | leskurMedSitteplass = func tiltakStates.leskurMedSitteplass
                    }
                )
        , nytteMultiplikator = verdisettinger.leskurPaaBussholdeplassenMedSitteplass
        , levetid = 12
        , title = "Leskur med sitteplass"
        , isHoldeplassTiltak = True
        }


sitteplassPaaHplTiltak : Tiltak
sitteplassPaaHplTiltak =
    SimpleTiltak.createTiltak
        { focus =
            Focus.create
                .sitteplassPaaHpl
                (\func tiltakStates ->
                    { tiltakStates
                        | sitteplassPaaHpl = func tiltakStates.sitteplassPaaHpl
                    }
                )
        , nytteMultiplikator = verdisettinger.sitteplassPaaHpl
        , levetid = 12
        , title = "Sitteplass på holdeplass"
        , isHoldeplassTiltak = True
        }


renholdPaaHplTiltak : Tiltak
renholdPaaHplTiltak =
    SuperSimpleTiltak.createTiltak
        { focus =
            Focus.create
                .renholdPaaHpl
                (\func tiltakStates ->
                    { tiltakStates
                        | renholdPaaHpl = func tiltakStates.renholdPaaHpl
                    }
                )
        , nytteMultiplikator = verdisettinger.renholdPaaHpl
        , title = "Renhold på holdeplass"
        }


fjerningAvIsSnoePaaHplTiltak : Tiltak
fjerningAvIsSnoePaaHplTiltak =
    SuperSimpleTiltak.createTiltak
        { focus =
            Focus.create
                .fjerningAvIsSnoePaaHpl
                (\func tiltakStates ->
                    { tiltakStates
                        | fjerningAvIsSnoePaaHpl = func tiltakStates.fjerningAvIsSnoePaaHpl
                    }
                )
        , nytteMultiplikator = verdisettinger.fjerningAvIsSnoePaaHpl
        , title = "Økt fjerning av is og snø på holdeplass"
        }


skiltingIBussTiltak : Tiltak
skiltingIBussTiltak =
    SimpleTiltak.createTiltak
        { focus =
            Focus.create
                .skiltingIBuss
                (\func tiltakStates ->
                    { tiltakStates
                        | skiltingIBuss = func tiltakStates.skiltingIBuss
                    }
                )
        , nytteMultiplikator = verdisettinger.skiltingIBuss
        , levetid = 7
        , title = "Elektronisk skilting i bussen av neste holdeplass"
        , isHoldeplassTiltak = False
        }


pakkeSkiltOgOppropBussTiltak : Tiltak
pakkeSkiltOgOppropBussTiltak =
    SimpleTiltak.createTiltak
        { focus =
            Focus.create
                .pakkeSkiltOgOppropBuss
                (\func tiltakStates ->
                    { tiltakStates
                        | pakkeSkiltOgOppropBuss = func tiltakStates.pakkeSkiltOgOppropBuss
                    }
                )
        , nytteMultiplikator = verdisettinger.pakkeSkiltOgOppropBuss
        , levetid = 7
        , title =
            "Elektronisk skilting og opprop i bussen av neste holdeplass"
        , isHoldeplassTiltak = False
        }


avviksinformasjonHoeyttalerTiltak : Tiltak
avviksinformasjonHoeyttalerTiltak =
    {-
         name = "passengersPerYear
       , placeholder = "Årlig antall påstigende passasjerer på holdeplassen"
    -}
    SimpleTiltak.createTiltak
        { focus =
            Focus.create
                .avviksinformasjonHoeyttaler
                (\func tiltakStates ->
                    { tiltakStates
                        | avviksinformasjonHoeyttaler =
                            func tiltakStates.avviksinformasjonHoeyttaler
                    }
                )
        , nytteMultiplikator = verdisettinger.avviksinformasjonHoeyttaler
        , levetid = 20
        , title = "Informasjon over høyttaler på holdeplass om avvik"
        , isHoldeplassTiltak = True
        }


belysningTiltak : Tiltak
belysningTiltak =
    SimpleTiltak.createTiltak
        { focus =
            Focus.create
                .belysning
                (\func tiltakStates ->
                    { tiltakStates
                        | belysning = func tiltakStates.belysning
                    }
                )
        , nytteMultiplikator = verdisettinger.belysning
        , levetid = 20
        , title = "Belysning på holdeplass"
        , isHoldeplassTiltak = True
        }


tiltakForGroup : Group -> List Tiltak
tiltakForGroup gruppeType =
    case gruppeType of
        Holdeplasser ->
            [ -- Sykkelparkering.tiltak
              {-
                 "Årlig
                 antall påstigende passasjerer på holdeplassen"
              -}
              SimpleTiltak.createTiltak
                { focus =
                    Focus.create
                        .leskurUtenSitteplass
                        (\func tiltakStates ->
                            { tiltakStates
                                | leskurUtenSitteplass = func tiltakStates.leskurUtenSitteplass
                            }
                        )
                , nytteMultiplikator =
                    verdisettinger.leskurPaaBussholdeplassenUtenSitteplass
                , levetid =
                    12
                , title = "Leskur uten sitteplass"
                , isHoldeplassTiltak = True
                }

            {-
               "Årlig
               antall påstigende passasjerer på holdeplassen"
            -}
            , sitteplassPaaHplTiltak

            {-
               "Årlig
               antall påstigende passasjerer på holdeplassen"
            -}
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
                { focus =
                    Focus.create
                        .destinasjonsSkiltPaaBuss
                        (\func tiltakStates ->
                            { tiltakStates
                                | destinasjonsSkiltPaaBuss =
                                    func tiltakStates.destinasjonsSkiltPaaBuss
                            }
                        )
                , nytteMultiplikator = verdisettinger.destinasjonsSkiltPaaBuss
                , levetid = 10
                , title = "Destinasjonsskilt bak og på siden av bussen"
                , isHoldeplassTiltak = False
                }
            , avviksinformasjonHoeyttalerTiltak
            ]

        Trygghet ->
            [ belysningTiltak
            , SimpleTiltak.createTiltak
                { focus =
                    Focus.create
                        .alarmsystemPaaHpl
                        (\func tiltakStates ->
                            { tiltakStates
                                | alarmsystemPaaHpl = func tiltakStates.alarmsystemPaaHpl
                            }
                        )
                , nytteMultiplikator = verdisettinger.alarmsystemPaaHpl
                , levetid = 20
                , title = "Alarmsystem / Nødtelefon på holdeplass"
                , isHoldeplassTiltak = True
                }
            , SuperSimpleTiltak.createTiltak
                { focus =
                    Focus.create
                        .vektere
                        (\func tiltakStates ->
                            { tiltakStates
                                | vektere = func tiltakStates.vektere
                            }
                        )
                , nytteMultiplikator = verdisettinger.vektere
                , title = "Vektere på holdeplass"
                }
            ]

        Kjoeremateriell ->
            [ Bussrenhold.tiltak

            -- buggy percent, issue #36 , Laventrebuss.tiltak
            ]

        StrekningOgFramkommelighet ->
            [ Kantsteinstopp.tiltak
            , KollektivPrioriteringLyskryss.tiltak

            -- buggy kalkulasjoner, issue #30 , KollektivPrioriteringSkilting.tiltak
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

            -- buggy percent, issue #36 , Laventrebuss.tiltak
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
