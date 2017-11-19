module Tiltak.KollektivPrioriteringSkilting exposing (..)

import Focus exposing (..)
import Tiltak exposing (Tiltak(..), StateCalculationMethod, sendTo)
import Field exposing (SimpleField, Field)
import BasicTiltak
import SpecificStates exposing (KollektivPrioriteringSkiltingState)
import FormattedValue
    exposing
        ( formattedValueDefault
        , installationCost
        , value
        , yearlyMaintenance
        , passengersPerYear
        )
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
        kollektivPrioriteringSkilting.passengersPerYear.value
        kollektivPrioriteringSkilting.forsinkelsePerBilSeconds.value


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
        kollektivPrioriteringSkilting.antallBilerForkjoersrettPerYear.value
        kollektivPrioriteringSkilting.tidsgevinstPerBilSeconds.value
        kollektivPrioriteringSkilting.antallBilerForsinketPerYear.value
        kollektivPrioriteringSkilting.forsinkelsePerBilSeconds.value


yearlyOperatoerNytte : StateCalculationMethod
yearlyOperatoerNytte this ({ kollektivPrioriteringSkilting } as state) =
    let
        calculation passerendeAvganger tidsgevinstPerBilSeconds =
            passerendeAvganger
                * (tidsgevinstPerBilSeconds / 60)
                * verdisettinger.operatoerKostnad
    in
        Maybe.map2 calculation
            kollektivPrioriteringSkilting.antallPasserendeAvgangerPerYear.value
            kollektivPrioriteringSkilting.tidsgevinstPerBilSeconds.value


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
            BasicTiltak.basicTiltakRecord specificStateFocus
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
    { installationCost = formattedValueDefault
    , yearlyMaintenance = formattedValueDefault
    , passengersPerYear = formattedValueDefault
    , bompengeAndel = 0
    , antallBilerForsinketPerYear = formattedValueDefault
    , forsinkelsePerBilSeconds = formattedValueDefault
    , antallBilerForkjoersrettPerYear = formattedValueDefault
    , tidsgevinstPerBilSeconds = formattedValueDefault
    , antallPasserendeAvgangerPerYear = formattedValueDefault
    , preferredToGraph = ""
    }


forsinkelsePerBilSeconds : Focus { b | forsinkelsePerBilSeconds : a } a
forsinkelsePerBilSeconds =
    Focus.create
        .forsinkelsePerBilSeconds
        (\f state ->
            { state
                | forsinkelsePerBilSeconds = f state.forsinkelsePerBilSeconds
            }
        )


antallBilerForkjoersrettPerYear : Focus { b | antallBilerForkjoersrettPerYear : a } a
antallBilerForkjoersrettPerYear =
    Focus.create
        .antallBilerForkjoersrettPerYear
        (\f state ->
            { state
                | antallBilerForkjoersrettPerYear = f state.antallBilerForkjoersrettPerYear
            }
        )


antallBilerForsinketPerYear : Focus { b | antallBilerForsinketPerYear : a } a
antallBilerForsinketPerYear =
    Focus.create
        .antallBilerForsinketPerYear
        (\f state ->
            { state
                | antallBilerForsinketPerYear = f state.antallBilerForsinketPerYear
            }
        )


tidsgevinstPerBilSeconds : Focus { b | tidsgevinstPerBilSeconds : a } a
tidsgevinstPerBilSeconds =
    Focus.create
        .tidsgevinstPerBilSeconds
        (\f state ->
            { state
                | tidsgevinstPerBilSeconds = f state.tidsgevinstPerBilSeconds
            }
        )


antallPasserendeAvgangerPerYear : Focus { b | antallPasserendeAvgangerPerYear : a } a
antallPasserendeAvgangerPerYear =
    Focus.create
        .antallPasserendeAvgangerPerYear
        (\f state ->
            { state
                | antallPasserendeAvgangerPerYear = f state.antallPasserendeAvgangerPerYear
            }
        )


fieldDefinitions : List SimpleField
fieldDefinitions =
    [ { name = "installationCost"
      , title = "Installasjonskostnad"
      , placeholder = "Kostnaden ved å installere tiltaket en gang, kroner"
      , focus = specificStateFocus => installationCost
      , stepSize = 50000
      }
    , { name = "yearlyMaintenance"
      , title = "Årlige drifts- og vedlikeholdskostnader"
      , placeholder = "Årlige drifts- og vedlikeholdskostnader, kroner"
      , focus = specificStateFocus => yearlyMaintenance
      , stepSize = 5000
      }
    , { name = "passengersPerYear"
      , title = "Antall passasjerer ombord per år"
      , placeholder = "Passasjerer ombord gjennom krysset"
      , focus = specificStateFocus => passengersPerYear
      , stepSize = 50
      }
    , { name = "antallBilerForsinketPerYear"
      , title = "Antall forsinkete biler per år"
      , placeholder = "Passerer krysset fra vei som får vikeplikt"
      , focus = specificStateFocus => antallBilerForsinketPerYear
      , stepSize = 1000
      }
    , { name = "forsinkelsePerBilSeconds"
      , title = "Sekunder forsinkelse per kjøretøy"
      , placeholder = "Når de blir forsinket hvor mange sekunder"
      , focus = specificStateFocus => forsinkelsePerBilSeconds
      , stepSize = 1
      }
    , { name = "antallBilerForkjoersrettPerYear"
      , title = "Antall biler som får forkjørsrett per år"
      , placeholder = "Passerer krysset fra vei som får forkjørsrett"
      , focus = specificStateFocus => antallBilerForkjoersrettPerYear
      , stepSize = 1000
      }
    , { name = "tidsgevinstPerBilSeconds"
      , title = "Sekunder tidsgevinst per kjøretøy"
      , placeholder = "Per kjøretøy sekunder"
      , focus = specificStateFocus => tidsgevinstPerBilSeconds
      , stepSize = 1
      }
    , { name = "antallPasserendeAvgangerPerYear"
      , title = "Avganger som passererer krysset med prioritering"
      , placeholder = "Prioterte avganger per år"
      , focus = specificStateFocus => antallPasserendeAvgangerPerYear
      , stepSize = 1000
      }
    ]


specificStateFocus : Focus { b | kollektivPrioriteringSkilting : a } a
specificStateFocus =
    Focus.create
        .kollektivPrioriteringSkilting
        (\f tiltakStates ->
            { tiltakStates
                | kollektivPrioriteringSkilting =
                    f tiltakStates.kollektivPrioriteringSkilting
            }
        )


fields : List Field
fields =
    fieldDefinitions
        |> Field.transformToFields
