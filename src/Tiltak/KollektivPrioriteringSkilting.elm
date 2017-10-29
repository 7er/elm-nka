module Tiltak.KollektivPrioriteringSkilting exposing (..)

import Tiltak exposing (Tiltak(..), StateCalculationMethod, sendTo)
import Field exposing (SimpleField, Field)
import BasicTiltak
import TiltakStates exposing (KollektivPrioriteringSkiltingState)
import GeneralForutsetninger exposing (verdisettinger)


levetid : number
levetid =
    10


yearlyPassasjerNytte : StateCalculationMethod
yearlyPassasjerNytte this ({ kollektivPrioriteringSkilting } as state) =
    Maybe.map2
        (\passengersPerYear forsinkelsePerBilSeconds ->
            (forsinkelsePerBilSeconds / 60)
                * verdisettinger.reisetidKollektivTransport
                * passengersPerYear
        )
        kollektivPrioriteringSkilting.passengersPerYear
        kollektivPrioriteringSkilting.forsinkelsePerBilSeconds


yearlyTrafikantNytte : StateCalculationMethod
yearlyTrafikantNytte this ({ kollektivPrioriteringSkilting } as state) =
    Maybe.map4
        (\antallBilerForkjoersrettPerYear tidsgevinstPerBilSeconds antallBilerForsinketPerYear forsinkelsePerBilSeconds ->
            (antallBilerForkjoersrettPerYear
                * (tidsgevinstPerBilSeconds / 60)
                * verdisettinger.reisetidBil
            )
                - (antallBilerForsinketPerYear
                    * (forsinkelsePerBilSeconds / 60)
                    * verdisettinger.reisetidBil
                  )
        )
        kollektivPrioriteringSkilting.antallBilerForkjoersrettPerYear
        kollektivPrioriteringSkilting.tidsgevinstPerBilSeconds
        kollektivPrioriteringSkilting.antallBilerForsinketPerYear
        kollektivPrioriteringSkilting.forsinkelsePerBilSeconds


yearlyOperatoerNytte : StateCalculationMethod
yearlyOperatoerNytte this ({ kollektivPrioriteringSkilting } as state) =
    let
        calculation passerendeAvganger tidsgevinstPerBilSeconds =
            passerendeAvganger
                * (tidsgevinstPerBilSeconds / 60)
                * verdisettinger.operatoerKostnad
    in
        Maybe.map2 calculation
            kollektivPrioriteringSkilting.antallPasserendeAvgangerPerYear
            kollektivPrioriteringSkilting.tidsgevinstPerBilSeconds


investeringsKostInklRestverdi : StateCalculationMethod
investeringsKostInklRestverdi this ({ kollektivPrioriteringSkilting } as state) =
    BasicTiltak.investeringsKostInklRestverdi
        kollektivPrioriteringSkilting
        levetid


driftOgVedlihKost : StateCalculationMethod
driftOgVedlihKost this ({ kollektivPrioriteringSkilting } as state) =
    BasicTiltak.driftOgVedlihKost kollektivPrioriteringSkilting


skyggepris : StateCalculationMethod
skyggepris this ({ kollektivPrioriteringSkilting } as state) =
    (sendTo this .skyggeprisHelper state kollektivPrioriteringSkilting.bompengeAndel)


tiltak : Tiltak
tiltak =
    let
        basicTiltakRecord =
            BasicTiltak.basicTiltakRecord
    in
        Tiltak
            { basicTiltakRecord
                | title = \_ -> "Kollektivprioritering ved skilting"
                , yearlyPassasjerNytte = yearlyPassasjerNytte
                , yearlyTrafikantNytte = yearlyTrafikantNytte
                , yearlyOperatoerNytte = yearlyOperatoerNytte
                , investeringsKostInklRestverdi = investeringsKostInklRestverdi
                , driftOgVedlihKost = driftOgVedlihKost
                , skyggepris = skyggepris
                , fields = \_ -> fields
            }


initialState : KollektivPrioriteringSkiltingState
initialState =
    { installationCost = Nothing
    , yearlyMaintenance = Nothing
    , passengersPerYear = Nothing
    , bompengeAndel = 0
    , antallBilerForsinketPerYear = Nothing
    , forsinkelsePerBilSeconds = Nothing
    , antallBilerForkjoersrettPerYear = Nothing
    , tidsgevinstPerBilSeconds = Nothing
    , antallPasserendeAvgangerPerYear = Nothing
    , preferredToGraph = ""
    }


fieldDefinitions : List (SimpleField KollektivPrioriteringSkiltingState)
fieldDefinitions =
    [ { name = "installationCost"
      , title = "Installasjonskostnad"
      , placeholder = "Kostnaden ved å installere tiltaket en gang, kroner"
      , setter =
            (\value state ->
                { state
                    | installationCost = value
                }
            )
      , accessor = .installationCost
      , stepSize = 50000
      }
    , { name = "yearlyMaintenance"
      , title = "Årlige drifts- og vedlikeholdskostnader"
      , placeholder = "Årlige drifts- og vedlikeholdskostnader, kroner"
      , setter =
            (\value state ->
                { state
                    | yearlyMaintenance = value
                }
            )
      , accessor = .yearlyMaintenance
      , stepSize = 5000
      }
    , { name = "passengersPerYear"
      , title = "Antall passasjerer ombord per år"
      , placeholder = "Passasjerer ombord gjennom krysset"
      , setter =
            (\value state ->
                { state
                    | passengersPerYear = value
                }
            )
      , accessor = .passengersPerYear
      , stepSize = 50
      }
    , { name = "antallBilerForsinketPerYear"
      , title = "Antall forsinkete biler per år"
      , placeholder = "Passerer krysset fra vei som får vikeplikt"
      , setter =
            (\value state ->
                { state
                    | antallBilerForsinketPerYear = value
                }
            )
      , accessor = .antallBilerForsinketPerYear
      , stepSize = 1000
      }
    , { name = "forsinkelsePerBilSeconds"
      , title = "Sekunder forsinkelse per kjøretøy"
      , placeholder = "Når de blir forsinket hvor mange sekunder"
      , setter =
            (\value state ->
                { state
                    | forsinkelsePerBilSeconds = value
                }
            )
      , accessor = .forsinkelsePerBilSeconds
      , stepSize = 1
      }
    , { name = "antallBilerForkjoersrettPerYear"
      , title = "Antall biler som får forkjørsrett per år"
      , placeholder = "Passerer krysset fra vei som får forkjørsrett"
      , setter =
            (\value state ->
                { state
                    | antallBilerForkjoersrettPerYear = value
                }
            )
      , accessor = .antallBilerForkjoersrettPerYear
      , stepSize = 1000
      }
    , { name = "tidsgevinstPerBilSeconds"
      , title = "Sekunder tidsgevinst per kjøretøy"
      , placeholder = "Per kjøretøy sekunder"
      , setter =
            (\value state ->
                { state
                    | tidsgevinstPerBilSeconds = value
                }
            )
      , accessor = .tidsgevinstPerBilSeconds
      , stepSize = 1
      }
    , { name = "antallPasserendeAvgangerPerYear"
      , title = "Avganger som passererer krysset med prioritering"
      , placeholder = "Prioterte avganger per år"
      , setter =
            (\value state ->
                { state
                    | antallPasserendeAvgangerPerYear = value
                }
            )
      , accessor = .antallPasserendeAvgangerPerYear
      , stepSize = 1000
      }
    ]


fields : List Field
fields =
    let
        stateMap func tiltakStates =
            { tiltakStates
                | kollektivPrioriteringSkilting = func tiltakStates.kollektivPrioriteringSkilting
            }

        updateTiltakStateHelper =
            TiltakStates.stateUpdateHelper stateMap

        thisValueHelper =
            TiltakStates.valueHelper .kollektivPrioriteringSkilting
    in
        fieldDefinitions
            |> Field.transformToFields
                stateMap
                updateTiltakStateHelper
                thisValueHelper
