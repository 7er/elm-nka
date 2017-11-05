module Tiltak.KollektivPrioriteringSkilting exposing (..)

import Focus exposing (..)
import Tiltak exposing (Tiltak(..), StateCalculationMethod, sendTo)
import Field exposing (SimpleField, Field)
import BasicTiltak
import TiltakStates
    exposing
        ( KollektivPrioriteringSkiltingState
        , formattedValueDefault
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


forsinkelsePerBilSeconds =
    Focus.create
        .forsinkelsePerBilSeconds
        (\f state ->
            { state
                | forsinkelsePerBilSeconds = f state.forsinkelsePerBilSeconds
            }
        )


antallBilerForkjoersrettPerYear =
    Focus.create
        .antallBilerForkjoersrettPerYear
        (\f state ->
            { state
                | antallBilerForkjoersrettPerYear = f state.antallBilerForkjoersrettPerYear
            }
        )


antallBilerForsinketPerYear =
    Focus.create
        .antallBilerForsinketPerYear
        (\f state ->
            { state
                | antallBilerForsinketPerYear = f state.antallBilerForsinketPerYear
            }
        )


tidsgevinstPerBilSeconds =
    Focus.create
        .tidsgevinstPerBilSeconds
        (\f state ->
            { state
                | tidsgevinstPerBilSeconds = f state.tidsgevinstPerBilSeconds
            }
        )


antallPasserendeAvgangerPerYear =
    Focus.create
        .antallPasserendeAvgangerPerYear
        (\f state ->
            { state
                | antallPasserendeAvgangerPerYear = f state.antallPasserendeAvgangerPerYear
            }
        )


fieldDefinitions : List (SimpleField KollektivPrioriteringSkiltingState)
fieldDefinitions =
    [ { name = "installationCost"
      , title = "Installasjonskostnad"
      , placeholder = "Kostnaden ved å installere tiltaket en gang, kroner"
      , setter = Focus.set (installationCost => value)
      , accessor = Focus.get (installationCost => value)
      , stepSize = 50000
      }
    , { name = "yearlyMaintenance"
      , title = "Årlige drifts- og vedlikeholdskostnader"
      , placeholder = "Årlige drifts- og vedlikeholdskostnader, kroner"
      , setter = Focus.set (yearlyMaintenance => value)
      , accessor = Focus.get (yearlyMaintenance => value)
      , stepSize = 5000
      }
    , { name = "passengersPerYear"
      , title = "Antall passasjerer ombord per år"
      , placeholder = "Passasjerer ombord gjennom krysset"
      , setter = Focus.set (passengersPerYear => value)
      , accessor = Focus.get (passengersPerYear => value)
      , stepSize = 50
      }
    , { name = "antallBilerForsinketPerYear"
      , title = "Antall forsinkete biler per år"
      , placeholder = "Passerer krysset fra vei som får vikeplikt"
      , setter = Focus.set (antallBilerForsinketPerYear => value)
      , accessor = Focus.get (antallBilerForsinketPerYear => value)
      , stepSize = 1000
      }
    , { name = "forsinkelsePerBilSeconds"
      , title = "Sekunder forsinkelse per kjøretøy"
      , placeholder = "Når de blir forsinket hvor mange sekunder"
      , setter = Focus.set (forsinkelsePerBilSeconds => value)
      , accessor = Focus.get (forsinkelsePerBilSeconds => value)
      , stepSize = 1
      }
    , { name = "antallBilerForkjoersrettPerYear"
      , title = "Antall biler som får forkjørsrett per år"
      , placeholder = "Passerer krysset fra vei som får forkjørsrett"
      , setter = Focus.set (antallBilerForkjoersrettPerYear => value)
      , accessor = Focus.get (antallBilerForkjoersrettPerYear => value)
      , stepSize = 1000
      }
    , { name = "tidsgevinstPerBilSeconds"
      , title = "Sekunder tidsgevinst per kjøretøy"
      , placeholder = "Per kjøretøy sekunder"
      , setter = Focus.set (tidsgevinstPerBilSeconds => value)
      , accessor = Focus.get (tidsgevinstPerBilSeconds => value)
      , stepSize = 1
      }
    , { name = "antallPasserendeAvgangerPerYear"
      , title = "Avganger som passererer krysset med prioritering"
      , placeholder = "Prioterte avganger per år"
      , setter = Focus.set (antallPasserendeAvgangerPerYear => value)
      , accessor = Focus.get (antallPasserendeAvgangerPerYear => value)
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
