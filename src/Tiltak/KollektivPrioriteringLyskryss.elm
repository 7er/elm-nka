module Tiltak.KollektivPrioriteringLyskryss exposing (..)

import Tiltak exposing (Tiltak(..), StateCalculationMethod, sendTo)
import Field exposing (SimpleField, Field)
import BasicTiltak
import TiltakStates exposing (KollektivPrioriteringLyskryssState)
import GeneralForutsetninger exposing (verdisettinger)


levetid : number
levetid =
    15


tidsbesparelsePerAvgangSeconds : number
tidsbesparelsePerAvgangSeconds =
    20


yearlyPassasjerNytte : StateCalculationMethod
yearlyPassasjerNytte this ({ kollektivPrioriteringLyskryss } as state) =
    kollektivPrioriteringLyskryss.passengersPerYear
        |> Maybe.map
            (\passengersPerYear ->
                (tidsbesparelsePerAvgangSeconds / 60)
                    * verdisettinger.reisetidKollektivTransport
                    * passengersPerYear
            )


yearlyTrafikantNytte : StateCalculationMethod
yearlyTrafikantNytte this ({ kollektivPrioriteringLyskryss } as state) =
    Maybe.map3
        (\antallBilerForsinketPerAvgang antallPasserendeAvgangerPerYear forsinkelsePerBilSeconds ->
            antallBilerForsinketPerAvgang
                * antallPasserendeAvgangerPerYear
                * (negate forsinkelsePerBilSeconds / 60)
                * verdisettinger.reisetidBil
        )
        kollektivPrioriteringLyskryss.antallBilerForsinketPerAvgang
        kollektivPrioriteringLyskryss.antallPasserendeAvgangerPerYear
        kollektivPrioriteringLyskryss.forsinkelsePerBilSeconds


yearlyOperatoerNytte : StateCalculationMethod
yearlyOperatoerNytte this ({ kollektivPrioriteringLyskryss } as state) =
    let
        calculation passerendeAvganger =
            passerendeAvganger
                * (tidsbesparelsePerAvgangSeconds / 60)
                * (verdisettinger).operatoerKostnad
    in
        kollektivPrioriteringLyskryss.antallPasserendeAvgangerPerYear
            |> Maybe.map calculation


investeringsKostInklRestverdi : StateCalculationMethod
investeringsKostInklRestverdi this ({ kollektivPrioriteringLyskryss } as state) =
    BasicTiltak.investeringsKostInklRestverdi
        kollektivPrioriteringLyskryss
        levetid


driftOgVedlihKost : StateCalculationMethod
driftOgVedlihKost this ({ kollektivPrioriteringLyskryss } as state) =
    BasicTiltak.driftOgVedlihKost kollektivPrioriteringLyskryss


skyggepris : StateCalculationMethod
skyggepris this ({ kollektivPrioriteringLyskryss } as state) =
    (sendTo this .skyggeprisHelper state kollektivPrioriteringLyskryss.bompengeAndel)


tiltak : Tiltak
tiltak =
    let
        basicTiltakRecord =
            BasicTiltak.basicTiltakRecord
    in
        Tiltak
            { basicTiltakRecord
                | title = \_ -> "Kollektivprioritering lyskryss"
                , yearlyPassasjerNytte = yearlyPassasjerNytte
                , yearlyTrafikantNytte = yearlyTrafikantNytte
                , yearlyOperatoerNytte = yearlyOperatoerNytte
                , investeringsKostInklRestverdi = investeringsKostInklRestverdi
                , driftOgVedlihKost = driftOgVedlihKost
                , skyggepris = skyggepris
                , fields = \_ -> fields
            }


initialState : KollektivPrioriteringLyskryssState
initialState =
    { installationCost = Nothing
    , yearlyMaintenance = Nothing
    , passengersPerYear = Nothing
    , bompengeAndel = 0
    , antallBilerForsinketPerAvgang = Nothing
    , forsinkelsePerBilSeconds = Nothing
    , antallPasserendeAvgangerPerYear = Nothing
    , preferredToGraph = ""
    }


fieldDefinitions : List (SimpleField KollektivPrioriteringLyskryssState)
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
      , placeholder = "Passasjerer ombord"
      , setter =
            (\value state ->
                { state
                    | passengersPerYear = value
                }
            )
      , accessor = .passengersPerYear
      , stepSize = 50
      }
    , { name = "antallBilerForsinketPerAvgang"
      , title = "Antall forsinkete biler per avgang"
      , placeholder = "Forsinkete biler på den kryssende veien"
      , setter =
            (\value state ->
                { state
                    | antallBilerForsinketPerAvgang = value
                }
            )
      , accessor = .antallBilerForsinketPerAvgang
      , stepSize = 1
      }
    , { name = "forsinkelsePerBilSeconds"
      , title = "Sekunder forsinkelse per bil"
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
    , { name = "antallPasserendeAvgangerPerYear"
      , title = "Avganger som passererer krysset"
      , placeholder = "Antall passerende avganger per år"
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
                | kollektivPrioriteringLyskryss = func tiltakStates.kollektivPrioriteringLyskryss
            }

        updateTiltakStateHelper =
            TiltakStates.stateUpdateHelper stateMap

        thisValueHelper =
            TiltakStates.valueHelper .kollektivPrioriteringLyskryss
    in
        fieldDefinitions
            |> Field.transformToFields
                stateMap
                updateTiltakStateHelper
                thisValueHelper
