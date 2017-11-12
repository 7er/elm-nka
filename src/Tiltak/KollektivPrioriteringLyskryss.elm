module Tiltak.KollektivPrioriteringLyskryss exposing (..)

import Focus exposing (..)
import Tiltak exposing (Tiltak(..), StateCalculationMethod, sendTo)
import Field exposing (SimpleField, Field)
import BasicTiltak
import TiltakStates
    exposing
        ( KollektivPrioriteringLyskryssState
        , passengersPerYear
        , installationCost
        , yearlyMaintenance
        , value
        , formattedValueDefault
        )
import GeneralForutsetninger exposing (verdisettinger)


levetid : number
levetid =
    15


tidsbesparelsePerAvgangSeconds : number
tidsbesparelsePerAvgangSeconds =
    20


yearlyPassasjerNytte : StateCalculationMethod
yearlyPassasjerNytte this tiltakStates =
    tiltakStates
        |> Focus.get (specificState => passengersPerYear => value)
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
        kollektivPrioriteringLyskryss.antallBilerForsinketPerAvgang.value
        kollektivPrioriteringLyskryss.antallPasserendeAvgangerPerYear.value
        kollektivPrioriteringLyskryss.forsinkelsePerBilSeconds.value


yearlyOperatoerNytte : StateCalculationMethod
yearlyOperatoerNytte this ({ kollektivPrioriteringLyskryss } as state) =
    let
        calculation passerendeAvganger =
            passerendeAvganger
                * (tidsbesparelsePerAvgangSeconds / 60)
                * (verdisettinger).operatoerKostnad
    in
        kollektivPrioriteringLyskryss.antallPasserendeAvgangerPerYear.value
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
                | title = \_ -> "Kollektivprioritering i lyskryss"
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
    { installationCost = formattedValueDefault
    , yearlyMaintenance = formattedValueDefault
    , passengersPerYear = formattedValueDefault
    , bompengeAndel = 0
    , antallBilerForsinketPerAvgang = formattedValueDefault
    , forsinkelsePerBilSeconds = formattedValueDefault
    , antallPasserendeAvgangerPerYear = formattedValueDefault
    , preferredToGraph = ""
    }


specificState : Focus { b | kollektivPrioriteringLyskryss : a } a
specificState =
    Focus.create
        .kollektivPrioriteringLyskryss
        (\f tiltakStates ->
            { tiltakStates
                | kollektivPrioriteringLyskryss =
                    f tiltakStates.kollektivPrioriteringLyskryss
            }
        )


antallBilerForsinketPerAvgang : Focus { b | antallBilerForsinketPerAvgang : a } a
antallBilerForsinketPerAvgang =
    Focus.create
        .antallBilerForsinketPerAvgang
        (\f state ->
            { state
                | antallBilerForsinketPerAvgang = f state.antallBilerForsinketPerAvgang
            }
        )


forsinkelsePerBilSeconds : Focus { b | forsinkelsePerBilSeconds : a } a
forsinkelsePerBilSeconds =
    Focus.create
        .forsinkelsePerBilSeconds
        (\f state ->
            { state
                | forsinkelsePerBilSeconds = f state.forsinkelsePerBilSeconds
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
      , focus = specificState => installationCost
      , stepSize = 50000
      }
    , { name = "yearlyMaintenance"
      , title = "Årlige drifts- og vedlikeholdskostnader"
      , placeholder = "Årlige drifts- og vedlikeholdskostnader, kroner"
      , focus = specificState => yearlyMaintenance
      , stepSize = 5000
      }
    , { name = "passengersPerYear"
      , title = "Antall passasjerer ombord per år"
      , placeholder = "Passasjerer ombord"
      , focus = specificState => passengersPerYear
      , stepSize = 50
      }
    , { name = "antallBilerForsinketPerAvgang"
      , title = "Antall forsinkete biler per avgang"
      , placeholder = "Forsinkete biler på den kryssende veien"
      , focus = specificState => antallBilerForsinketPerAvgang
      , stepSize = 1
      }
    , { name = "forsinkelsePerBilSeconds"
      , title = "Sekunder forsinkelse per bil"
      , placeholder = "Når de blir forsinket hvor mange sekunder"
      , focus = specificState => forsinkelsePerBilSeconds
      , stepSize = 1
      }
    , { name = "antallPasserendeAvgangerPerYear"
      , title = "Avganger som passererer krysset"
      , placeholder = "Antall passerende avganger per år"
      , focus = specificState => antallPasserendeAvgangerPerYear
      , stepSize = 1000
      }
    ]


fields : List Field
fields =
    fieldDefinitions
        |> Field.transformToFields
