module Tiltak.OpphoeyetHoldeplass exposing (tiltak, initialState)

import Tiltak exposing (Tiltak(..), sendTo)
import Tiltak.BasicTiltak as BasicTiltak
import GeneralForutsetninger


yearlyPassasjerNytte this ({ opphoeyetHoldeplass } as state) =
    let
        verdisettinger =
            GeneralForutsetninger.verdisettinger

        firstCalc passengersPerYear =
            passengersPerYear * verdisettinger.opphoyetHoldeplass

        first =
            Maybe.map firstCalc opphoeyetHoldeplass.passengersPerYear

        secondCalc beleggForbiPassasjererPerBuss aarligTidsbesparelseMinutter =
            beleggForbiPassasjererPerBuss
                * aarligTidsbesparelseMinutter
                * verdisettinger.reisetidKollektivTransport

        second =
            Maybe.map2
                secondCalc
                opphoeyetHoldeplass.beleggForbiPassasjererPerBuss
                opphoeyetHoldeplass.aarligTidsbesparelseMinutter
    in
        Maybe.map2 (+) first second


yearlyOperatoerNytte this ({ opphoeyetHoldeplass } as state) =
    let
        verdisettinger =
            GeneralForutsetninger.verdisettinger
    in
        Maybe.map (\minutter -> minutter * verdisettinger.operatoerKostnad) opphoeyetHoldeplass.aarligTidsbesparelseMinutter


levetid =
    25


tiltak : Tiltak
tiltak =
    let
        basicTiltakRecord =
            BasicTiltak.basicTiltakRecord
    in
        Tiltak
            { basicTiltakRecord
                | title = \_ -> "Opphøyet holdeplass"
                , yearlyPassasjerNytte = yearlyPassasjerNytte
                , yearlyOperatoerNytte = yearlyOperatoerNytte
                , investeringsKostInklRestverdi =
                    \_ { opphoeyetHoldeplass } ->
                        BasicTiltak.investeringsKostInklRestverdi
                            opphoeyetHoldeplass
                            levetid
                , driftOgVedlihKost =
                    \_ { opphoeyetHoldeplass } ->
                        BasicTiltak.driftOgVedlihKost opphoeyetHoldeplass
                , skyggepris =
                    \this ({ opphoeyetHoldeplass } as state) ->
                        sendTo this .skyggeprisHelper state opphoeyetHoldeplass.bompengeAndel
            }


initialState =
    { aarligAntallAvganger = Nothing
    , aarligTidsbesparelseMinutter = Nothing
    , beleggForbiPassasjererPerBuss = Nothing
    , bompengeAndel = 0
    , installationCost = Nothing
    , passengersPerYear = Nothing
    , yearlyMaintenance = Nothing
    }
