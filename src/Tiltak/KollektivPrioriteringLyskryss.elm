module Tiltak.KollektivPrioriteringLyskryss exposing (..)

import Focus exposing (..)
import Tiltak exposing (Tiltak(..), StateCalculationMethod, sendTo)
import Field exposing (SimpleField, Field)
import BasicTiltak
import SpecificStates exposing (KollektivPrioriteringLyskryssState)
import FormattedValue
    exposing
        ( passengersPerYear
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
        |> Focus.get (specificStateFocus => passengersPerYear => value)
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
            BasicTiltak.basicTiltakRecord specificStateFocus
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


specificStateFocus : Focus { b | kollektivPrioriteringLyskryss : a } a
specificStateFocus =
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
      , focus = specificStateFocus => installationCost
      , stepSize = 50000
      }
    , { name = "yearlyMaintenance"
      , title = "Årlige drifts- og vedlikeholdskostnader"
      , placeholder = BasicTiltak.yearlyMaintenancePlaceholder
      , focus = specificStateFocus => yearlyMaintenance
      , stepSize = 5000
      }
    , { name = "passengersPerYear"
      , title = "Antall passasjerer ombord per år"
      , placeholder = "Antall kollektivpassasjerer som omfattes av tiltaket"
      , focus = specificStateFocus => passengersPerYear
      , stepSize = 50
      }
    , { name = "antallBilerForsinketPerAvgang"
      , title = "Antall forsinkete biler per avgang"
      , placeholder = "Antall biler som forsinkes per avgang"
      , focus = specificStateFocus => antallBilerForsinketPerAvgang
      , stepSize = 1
      }
    , { name = "forsinkelsePerBilSeconds"
      , title = "Sekunder forsinkelse per bil"
      , placeholder = "Når de blir forsinket, antall sekunder"
      , focus = specificStateFocus => forsinkelsePerBilSeconds
      , stepSize = 1
      }
    , { name = "antallPasserendeAvgangerPerYear"
      , title = "Avganger som passererer krysset"
      , placeholder = "Antall passerende avganger per år"
      , focus = specificStateFocus => antallPasserendeAvgangerPerYear
      , stepSize = 1000
      }
    ]


fields : List Field
fields =
    fieldDefinitions
        |> Field.transformToFields
