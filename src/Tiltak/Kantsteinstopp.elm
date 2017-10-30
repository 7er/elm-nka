module Tiltak.Kantsteinstopp exposing (..)

import Tiltak exposing (Tiltak(..), StateCalculationMethod, sendTo)
import Field exposing (Field, SimpleField)
import TiltakStates exposing (TiltakStates, KantsteinstoppState)
import BasicTiltak
import GeneralForutsetninger exposing (verdisettinger)


tidsgevinstPerBussavgangSeconds =
    5


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
        kantsteinstopp.passengersPerYear


yearlyOperatoerNytte : StateCalculationMethod
yearlyOperatoerNytte this ({ kantsteinstopp } as state) =
    let
        func antallBussavgangerPerYear =
            antallBussavgangerPerYear
                * (tidsgevinstPerBussavgangSeconds / 60)
                * verdisettinger.operatoerKostnad
    in
        kantsteinstopp.antallBussavgangerPerYear
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
            kantsteinstopp.antallBilerForsinketPerAvgang
            kantsteinstopp.antallBussavgangerPerYear


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
    { installationCost = Nothing
    , yearlyMaintenance = Nothing
    , bompengeAndel = 0
    , passengersPerYear = Nothing
    , antallBilerForsinketPerAvgang = Nothing
    , antallBussavgangerPerYear = Nothing
    , preferredToGraph = ""
    }


fieldDefinitions : List (SimpleField KantsteinstoppState)
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
      , title = "Antall passasjerer om bord og på holdeplass per år"
      , placeholder = "Passasjerer per år"
      , setter =
            (\value state ->
                { state
                    | passengersPerYear = value
                }
            )
      , accessor = .passengersPerYear
      , stepSize = 1000
      }
    , { name = "antallBilerForsinketPerAvgang"
      , title = "Antall biler som forsinkes per avgang"
      , placeholder = "Forsinkete biler per avgang"
      , setter =
            (\value state ->
                { state
                    | antallBilerForsinketPerAvgang = value
                }
            )
      , accessor = .antallBilerForsinketPerAvgang
      , stepSize = 2
      }
    , { name = "antallBussavgangerPerYear"
      , title = "Antall bussavganger som bruker holdeplassen per år"
      , placeholder = "Bussavganger per år"
      , setter =
            (\value state ->
                { state
                    | antallBussavgangerPerYear = value
                }
            )
      , accessor = .antallBussavgangerPerYear
      , stepSize = 1000
      }
    ]


fields : List Field
fields =
    let
        stateMap updater tiltakStates =
            { tiltakStates
                | kantsteinstopp = updater tiltakStates.kantsteinstopp
            }

        updateTiltakStateHelper =
            TiltakStates.stateUpdateHelper stateMap

        thisValueHelper =
            TiltakStates.valueHelper .kantsteinstopp
    in
        fieldDefinitions
            |> Field.transformToFields
                stateMap
                updateTiltakStateHelper
                thisValueHelper
