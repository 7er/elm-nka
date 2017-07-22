module Tiltak.LeskurMedSitteplass exposing (tiltak, initialState)

import GeneralForutsetninger
import Tiltak exposing (TiltakNg(..), Field, sendTo)
import TiltakStates


passasjerNytte this ({ leskurMedSitteplass } as state) =
    (sendTo this .yearlyPassasjerNytte state) |> Maybe.map ((*) GeneralForutsetninger.afaktorVekst)


yearlyPassasjerNytte this ({ leskurMedSitteplass } as state) =
    leskurMedSitteplass.passengersPerYear |> Maybe.map toFloat |> Maybe.map ((*) GeneralForutsetninger.leskurPaaBussholdeplassenMedSitteplassNOK)


tiltak : TiltakNg
tiltak =
    TiltakNg
        { title = \_ -> "Leskur u sitteplass"
        , fields = \_ -> fields
        , passasjerNytte = passasjerNytte
        , kostUtenSkyggepris = \(TiltakNg object) state -> Nothing
        , nettoNytte = \(TiltakNg object) state -> Nothing
        , nytte = \(TiltakNg object) state -> Nothing
        , operatoerNytte = \_ _ -> Just 0
        , skyggePris = \(TiltakNg object) state -> Nothing
        , trafikantNytte = \_ _ -> Just 0
        , yearlyPassasjerNytte = yearlyPassasjerNytte
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


initialState =
    { installationCost = Nothing
    , yearlyMaintenance = Nothing
    , passengersPerYear = Nothing
    }
