module Tiltak.BasicTiltak exposing (..)

import Tiltak exposing (Tiltak(..), TiltakRecord, Field, sendTo, StateCalculationMethod, bindTiltak)
import GeneralForutsetninger


nytte : StateCalculationMethod
nytte this state =
    let
        f accessor =
            sendTo this accessor state
    in
        Maybe.map3
            (\a b c ->
                a + b + c
            )
            (f .passasjerNytte)
            (f .trafikantNytte)
            (f .operatoerNytte)


nettoNytte : StateCalculationMethod
nettoNytte this state =
    let
        f =
            bindTiltak this state
    in
        Maybe.map3 (\a b c -> a + b + c)
            (f .nytte)
            (f .kostUtenSkyggepris)
            (f .skyggepris)


passasjerNytte : StateCalculationMethod
passasjerNytte this state =
    (sendTo this .yearlyPassasjerNytte state) |> Maybe.map ((*) GeneralForutsetninger.afaktorVekst)


kostUtenSkyggepris : StateCalculationMethod
kostUtenSkyggepris this state =
    let
        f =
            bindTiltak this state
    in
        Maybe.map2 (+)
            (f .investeringsKostInklRestverdi)
            (f .driftOgVedlihKost)


basicTiltakRecord : TiltakRecord
basicTiltakRecord =
    { title = \_ -> "Basic tiltak"
    , fields = \_ -> []
    , passasjerNytte = \_ _ -> Nothing
    , kostUtenSkyggepris = kostUtenSkyggepris
    , nettoNytte = nettoNytte
    , nytte = nytte
    , operatoerNytte = \_ _ -> Just 0
    , skyggepris = \_ _ -> Nothing
    , trafikantNytte = \_ _ -> Just 0
    , yearlyPassasjerNytte = \_ _ -> Nothing
    , driftOgVedlihKost = \_ _ -> Nothing
    , investeringsKostInklRestverdi = \_ _ -> Nothing
    }
