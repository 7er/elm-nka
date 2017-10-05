module Tiltak.BasicTiltak exposing (..)

import Tiltak exposing (..)
import TiltakCharting
import TiltakStates exposing (TiltakStates)
import GeneralForutsetninger


toDomId : String -> String
toDomId string =
    string |> String.words |> String.join "-"


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
passasjerNytte =
    analysePeriodeNytteFor .yearlyPassasjerNytte


trafikantNytte : StateCalculationMethod
trafikantNytte =
    analysePeriodeNytteFor .yearlyTrafikantNytte


analysePeriodeNytteFor :
    Tiltak.TiltakAccessor (tiltakStates -> Maybe Float)
    -> Tiltak
    -> tiltakStates
    -> Maybe Float
analysePeriodeNytteFor accessor this state =
    (sendTo this accessor state) |> Maybe.map ((*) GeneralForutsetninger.afaktorVekst)


operatoerNytte : StateCalculationMethod
operatoerNytte =
    analysePeriodeNytteFor .yearlyOperatoerNytte


kostUtenSkyggepris : StateCalculationMethod
kostUtenSkyggepris this state =
    let
        f =
            bindTiltak this state
    in
        Maybe.map2 (+)
            (f .investeringsKostInklRestverdi)
            (f .driftOgVedlihKost)


skyggeprisHelper : Tiltak -> TiltakStates -> Float -> Maybe Float
skyggeprisHelper this state bompengeAndel =
    let
        calculation kostUtenSkyggepris =
            (1 - bompengeAndel)
                * kostUtenSkyggepris
                * GeneralForutsetninger.skyggepris
    in
        (sendTo this .kostUtenSkyggepris state)
            |> Maybe.map calculation


basicTiltakRecord : TiltakRecord
basicTiltakRecord =
    { title = \_ -> "Basic tiltak"
    , fields = \_ -> []
    , passasjerNytte = passasjerNytte
    , trafikantNytte = trafikantNytte
    , operatoerNytte = operatoerNytte
    , kostUtenSkyggepris = kostUtenSkyggepris
    , nettoNytte = nettoNytte
    , nytte = nytte
    , skyggepris = \_ _ -> Nothing
    , skyggeprisHelper = skyggeprisHelper
    , yearlyPassasjerNytte = \_ _ -> Nothing
    , yearlyTrafikantNytte = \_ _ -> Just 0
    , yearlyOperatoerNytte = \_ _ -> Just 0
    , driftOgVedlihKost = \_ _ -> Nothing
    , investeringsKostInklRestverdi = \_ _ -> Nothing
    , graphId = \this -> sendTo this .domId |> (++) "c3graph"
    , domId = \this -> sendTo this .title |> toDomId
    , graphState = TiltakCharting.graphState
    , graphData = TiltakCharting.graphData
    }


investeringsKostInklRestverdi :
    { a | installationCost : Maybe Float }
    -> Float
    -> Maybe Float
investeringsKostInklRestverdi record levetid =
    record.installationCost
        |> Maybe.map ((*) <| GeneralForutsetninger.investeringsFaktor levetid)
        |> Maybe.map negate


driftOgVedlihKost : { a | yearlyMaintenance : Maybe Float } -> Maybe Float
driftOgVedlihKost record =
    record.yearlyMaintenance
        |> Maybe.map ((*) GeneralForutsetninger.afaktor)
        |> Maybe.map negate
