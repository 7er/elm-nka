module Tiltak.Kantsteinstopp exposing (..)

import Focus exposing ((=>))
import Tiltak exposing (Tiltak(..), StateCalculationMethod, sendTo)
import Field exposing (Field, SimpleField)
import TiltakStates
    exposing
        ( TiltakStates
        , KantsteinstoppState
        , installationCost
        , value
        , yearlyMaintenance
        , passengersPerYear
        , formattedValueDefault
        )
import BasicTiltak
import GeneralForutsetninger exposing (verdisettinger)


tidsgevinstPerBussavgangSeconds : number
tidsgevinstPerBussavgangSeconds =
    5


bilenesGjennomsnittsForsinkelseSeconds : Float
bilenesGjennomsnittsForsinkelseSeconds =
    12.5


yearlyPassasjerNytte : StateCalculationMethod
yearlyPassasjerNytte this ({ kantsteinstopp } as state) =
    Maybe.map
        (\passengersPerYear ->
            (tidsgevinstPerBussavgangSeconds / 60)
                * passengersPerYear
                * verdisettinger.reisetidKollektivTransport
        )
        kantsteinstopp.passengersPerYear.value


yearlyOperatoerNytte : StateCalculationMethod
yearlyOperatoerNytte this ({ kantsteinstopp } as state) =
    let
        func antallBussavgangerPerYear =
            antallBussavgangerPerYear
                * (tidsgevinstPerBussavgangSeconds / 60)
                * verdisettinger.operatoerKostnad
    in
        kantsteinstopp.antallBussavgangerPerYear.value
            |> Maybe.map func


yearlyTrafikantNytte : StateCalculationMethod
yearlyTrafikantNytte this ({ kantsteinstopp } as state) =
    let
        func antallBilerForsinketPerAvgang antallBussavgangerPerYear =
            antallBilerForsinketPerAvgang
                * antallBussavgangerPerYear
                * (bilenesGjennomsnittsForsinkelseSeconds / 60)
                * verdisettinger.reisetidBil
                |> negate
    in
        Maybe.map2 func
            kantsteinstopp.antallBilerForsinketPerAvgang.value
            kantsteinstopp.antallBussavgangerPerYear.value


levetid : number
levetid =
    40


tiltak : Tiltak
tiltak =
    let
        basicTiltakRecord =
            BasicTiltak.basicTiltakRecord
    in
        Tiltak
            { basicTiltakRecord
                | title = \_ -> "Omgjøre busslomme til kantsteinstopp"
                , fields = \_ -> fields
                , yearlyPassasjerNytte = yearlyPassasjerNytte
                , yearlyOperatoerNytte = yearlyOperatoerNytte
                , yearlyTrafikantNytte = yearlyTrafikantNytte
                , investeringsKostInklRestverdi =
                    \_ { kantsteinstopp } ->
                        BasicTiltak.investeringsKostInklRestverdi
                            kantsteinstopp
                            levetid
                , driftOgVedlihKost =
                    \_ { kantsteinstopp } ->
                        BasicTiltak.driftOgVedlihKost kantsteinstopp
                , skyggepris =
                    \this ({ kantsteinstopp } as state) ->
                        sendTo
                            this
                            .skyggeprisHelper
                            state
                            kantsteinstopp.bompengeAndel
            }


initialState : KantsteinstoppState
initialState =
    { installationCost = formattedValueDefault
    , yearlyMaintenance = formattedValueDefault
    , bompengeAndel = 0
    , passengersPerYear = formattedValueDefault
    , antallBilerForsinketPerAvgang = formattedValueDefault
    , antallBussavgangerPerYear = formattedValueDefault
    , preferredToGraph = ""
    }


specificState =
    Focus.create
        .kantsteinstopp
        (\f tiltakStates ->
            { tiltakStates
                | kantsteinstopp = f tiltakStates.kantsteinstopp
            }
        )


fieldDefinitions : List (SimpleField KantsteinstoppState)
fieldDefinitions =
    let
        antallBilerForsinketPerAvgang =
            Focus.create
                .antallBilerForsinketPerAvgang
                (\f state ->
                    { state
                        | antallBilerForsinketPerAvgang = f state.antallBilerForsinketPerAvgang
                    }
                )

        antallBussavgangerPerYear =
            Focus.create
                .antallBussavgangerPerYear
                (\f state ->
                    { state
                        | antallBussavgangerPerYear = f state.antallBussavgangerPerYear
                    }
                )
    in
        [ { name = "installationCost"
          , title = "Installasjonskostnad"
          , placeholder = "Kostnaden ved å installere tiltaket en gang, kroner"
          , setter = Focus.set (installationCost => value)
          , accessor = Focus.get (installationCost => value)
          , focus = specificState => installationCost
          , stepSize = 50000
          }
        , { name = "yearlyMaintenance"
          , title = "Årlige drifts- og vedlikeholdskostnader"
          , placeholder = "Årlige drifts- og vedlikeholdskostnader, kroner"
          , setter = Focus.set (yearlyMaintenance => value)
          , accessor = Focus.get (yearlyMaintenance => value)
          , focus = specificState => yearlyMaintenance
          , stepSize = 5000
          }
        , { name = "passengersPerYear"
          , title = "Antall passasjerer om bord og på holdeplass per år"
          , placeholder = "Passasjerer per år"
          , setter = Focus.set (passengersPerYear => value)
          , accessor = Focus.get (passengersPerYear => value)
          , focus = specificState => passengersPerYear
          , stepSize = 1000
          }
        , { name = "antallBilerForsinketPerAvgang"
          , title = "Antall biler som forsinkes per avgang"
          , placeholder = "Forsinkete biler per avgang"
          , setter = Focus.set (antallBilerForsinketPerAvgang => value)
          , accessor = Focus.get (antallBilerForsinketPerAvgang => value)
          , focus = specificState => antallBilerForsinketPerAvgang
          , stepSize = 2
          }
        , { name = "antallBussavgangerPerYear"
          , title = "Antall bussavganger som bruker holdeplassen per år"
          , placeholder = "Bussavganger per år"
          , setter = Focus.set (antallBussavgangerPerYear => value)
          , accessor = Focus.get (antallBussavgangerPerYear => value)
          , focus = specificState => antallBussavgangerPerYear
          , stepSize = 1000
          }
        ]


fields : List Field
fields =
    fieldDefinitions
        |> Field.transformToFields
