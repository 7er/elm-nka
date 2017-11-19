module TiltakStates exposing (..)

import Focus exposing (Focus, (=>))
import SpecificStates exposing (..)


type alias TiltakStates =
    { leskurUtenSitteplass : SimpleCommonState
    , leskurMedSitteplass : SimpleCommonState
    , skiltingIBuss : SimpleCommonState
    , belysning : SimpleCommonState
    , sitteplassPaaHpl : SimpleCommonState
    , lokalkartPaaHpl : SimpleCommonState
    , rutekartPaaHpl : SimpleCommonState
    , pakkeSkiltOgOppropBuss : SimpleCommonState
    , destinasjonsSkiltPaaBuss : SimpleCommonState
    , avviksinformasjonHoeyttaler : SimpleCommonState
    , alarmsystemPaaHpl : SimpleCommonState
    , kollektivPrioriteringLyskryss : KollektivPrioriteringLyskryssState
    , opphoeyetHoldeplass : OpphoeyetHoldeplassState
    , renholdPaaHpl : SuperSimpleCommonState
    , fjerningAvIsSnoePaaHpl : SuperSimpleCommonState
    , vektere : SuperSimpleCommonState
    , hplOpprop : HplOppropState
    , kollektivPrioriteringSkilting : KollektivPrioriteringSkiltingState
    , bussrenhold : BussrenholdState
    , laventrebuss : LaventrebussState
    , kantsteinstopp : KantsteinstoppState
    }
