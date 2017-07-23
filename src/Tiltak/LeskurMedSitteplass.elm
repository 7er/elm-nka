module Tiltak.LeskurMedSitteplass exposing (tiltak, initialState)

import GeneralForutsetninger
import Tiltak exposing (TiltakNg(..), Field, sendTo, StateCalculationMethod, bindTiltak)
import TiltakStates exposing (TiltakStates)


passasjerNytte : StateCalculationMethod
passasjerNytte this state =
    (sendTo this .yearlyPassasjerNytte state) |> Maybe.map ((*) GeneralForutsetninger.afaktorVekst)


yearlyPassasjerNytte : StateCalculationMethod
yearlyPassasjerNytte this ({ leskurMedSitteplass } as state) =
    leskurMedSitteplass.passengersPerYear |> Maybe.map toFloat |> Maybe.map ((*) GeneralForutsetninger.leskurPaaBussholdeplassenMedSitteplassNOK)


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


kostUtenSkyggepris : StateCalculationMethod
kostUtenSkyggepris this state =
    let
        f =
            bindTiltak this state
    in
        Maybe.map2 (+)
            (f .investeringsKostInklRestverdi)
            (f .driftOgVedlihKost)


driftOgVedlihKost : StateCalculationMethod
driftOgVedlihKost this ({ leskurMedSitteplass } as state) =
    leskurMedSitteplass.yearlyMaintenance
        |> Maybe.map ((*) GeneralForutsetninger.afaktor)
        |> Maybe.map negate


levetid =
    12


investeringsKostInklRestverdi : StateCalculationMethod
investeringsKostInklRestverdi this ({ leskurMedSitteplass } as state) =
    leskurMedSitteplass.installationCost
        |> Maybe.map
            (\installationCost -> installationCost * GeneralForutsetninger.investeringsFaktor levetid)
        |> Maybe.map negate


skyggepris : StateCalculationMethod
skyggepris this ({ leskurMedSitteplass } as state) =
    (sendTo this .kostUtenSkyggepris state)
        |> Maybe.map
            (\kostUtenSkyggepris ->
                (1 - leskurMedSitteplass.bompengeAndel) * kostUtenSkyggepris * GeneralForutsetninger.skyggepris
            )


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


tiltak : TiltakNg
tiltak =
    TiltakNg
        { title = \_ -> "Leskur u sitteplass"
        , fields = \_ -> fields
        , passasjerNytte = passasjerNytte
        , kostUtenSkyggepris = kostUtenSkyggepris
        , nettoNytte = nettoNytte
        , nytte = nytte
        , operatoerNytte = \_ _ -> Just 0
        , skyggepris = skyggepris
        , trafikantNytte = \_ _ -> Just 0
        , yearlyPassasjerNytte = yearlyPassasjerNytte
        , driftOgVedlihKost = driftOgVedlihKost
        , investeringsKostInklRestverdi = investeringsKostInklRestverdi
        }


fields : List Field
fields =
    let
        stateMap func tiltakStates =
            { tiltakStates
                | leskurMedSitteplass = func tiltakStates.leskurMedSitteplass
            }

        updateTiltakStateHelper =
            TiltakStates.stateUpdateHelper stateMap

        thisStringValueHelper =
            TiltakStates.stringValueHelper .leskurMedSitteplass
    in
        [ { name = "passengersPerYear"
          , title = "Antall passasjerer per år"
          , placeholder = "Påstigende passasjerer per år"
          , updateTiltakState =
                updateTiltakStateHelper
                    (\stringValue state ->
                        { state
                            | passengersPerYear =
                                String.toInt stringValue |> Result.toMaybe
                        }
                    )
          , stringValueFromState = thisStringValueHelper .passengersPerYear
          }
        , { name = "installationCost"
          , title = "Installasjonskostnad"
          , placeholder = "Kostnaden ved å installere tiltaket en gang, kroner"
          , updateTiltakState =
                updateTiltakStateHelper
                    (\stringValue state ->
                        { state
                            | installationCost =
                                String.toFloat stringValue |> Result.toMaybe
                        }
                    )
          , stringValueFromState = thisStringValueHelper .installationCost
          }
        , { name = "yearlyMaintenance"
          , title = "Årlige drifts- og vedlikeholdskostnader"
          , placeholder = "Årlige drifts- og vedlikeholdskostnader, kroner"
          , updateTiltakState =
                updateTiltakStateHelper
                    (\stringValue state ->
                        { state
                            | yearlyMaintenance =
                                String.toFloat stringValue |> Result.toMaybe
                        }
                    )
          , stringValueFromState = thisStringValueHelper .yearlyMaintenance
          }
        ]


initialState : TiltakStates.SimpleCommonState
initialState =
    { installationCost = Nothing
    , yearlyMaintenance = Nothing
    , passengersPerYear = Nothing
    , bompengeAndel = 0.2
    }
