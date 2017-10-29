module Tiltak.Laventrebuss exposing (..)

import Tiltak exposing (Tiltak(..), StateCalculationMethod, sendTo)
import Field exposing (Field, SimpleField)
import TiltakStates exposing (TiltakStates, LaventrebussState)
import BasicTiltak
import GeneralForutsetninger exposing (verdisettinger)


yearlyPassasjerNytte : StateCalculationMethod
yearlyPassasjerNytte this ({ laventrebuss } as state) =
    let
        firstCalc =
            Maybe.map2
                (\passengersPerYear passasjererTilpassedeHoldplasserPercent ->
                    passengersPerYear
                        * passasjererTilpassedeHoldplasserPercent
                        * verdisettinger.lavgulvMedTilpassetHoldeplass
                )
                laventrebuss.passengersPerYear
                laventrebuss.passasjererTilpassedeHoldplasserPercent

        secondCalc =
            Maybe.map2
                (\passengersPerYear passasjererTilpassedeHoldplasserPercent ->
                    passengersPerYear
                        * (1 - passasjererTilpassedeHoldplasserPercent)
                        * verdisettinger.lavgulvUtenTilpassetHoldeplass
                )
                laventrebuss.passengersPerYear
                laventrebuss.passasjererTilpassedeHoldplasserPercent

        thirdCalc =
            Maybe.map2
                (\passasjererPerBuss yearlyTidsbesparelseMinutter ->
                    passasjererPerBuss
                        * yearlyTidsbesparelseMinutter
                        * verdisettinger.reisetidKollektivTransport
                )
                laventrebuss.passasjererPerBuss
                laventrebuss.yearlyTidsbesparelseMinutter
    in
        Maybe.map3
            (\first second third ->
                first
                    + second
                    + third
            )
            firstCalc
            secondCalc
            thirdCalc


yearlyOperatoerNytte : StateCalculationMethod
yearlyOperatoerNytte this ({ laventrebuss } as state) =
    Maybe.map2 (*)
        (Just verdisettinger.operatoerKostnad)
        laventrebuss.yearlyTidsbesparelseMinutter


levetid : number
levetid =
    10


tiltak : Tiltak
tiltak =
    let
        basicTiltakRecord =
            BasicTiltak.basicTiltakRecord
    in
        Tiltak
            { basicTiltakRecord
                | title = \_ -> "Lavgulv-/Laventre-busser"
                , fields = \_ -> fields
                , yearlyPassasjerNytte = yearlyPassasjerNytte
                , yearlyOperatoerNytte = yearlyOperatoerNytte
                , investeringsKostInklRestverdi =
                    \_ { laventrebuss } ->
                        BasicTiltak.investeringsKostInklRestverdi
                            laventrebuss
                            levetid
                , driftOgVedlihKost =
                    \_ { laventrebuss } ->
                        BasicTiltak.driftOgVedlihKost laventrebuss
                , skyggepris =
                    \this ({ laventrebuss } as state) ->
                        sendTo
                            this
                            .skyggeprisHelper
                            state
                            laventrebuss.bompengeAndel
            }


initialState : LaventrebussState
initialState =
    { installationCost = Nothing
    , yearlyMaintenance = Nothing
    , bompengeAndel = 0
    , passengersPerYear = Nothing
    , passasjererPerBuss = Nothing
    , yearlyTidsbesparelseMinutter = Nothing
    , passasjererTilpassedeHoldplasserPercent = Nothing
    , preferredToGraph = ""
    }


fieldDefinitions : List (SimpleField LaventrebussState)
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
      , title = "Antall passasjerer på hele ruta per år"
      , placeholder = "Passasjerer per år"
      , setter =
            (\value state ->
                { state
                    | passengersPerYear = value
                }
            )
      , accessor = .passengersPerYear
      , stepSize = 50
      }
    , { name = "passasjererPerBuss"
      , title = "Gjennomsnittsbelegg på bussene"
      , placeholder = "Passasjerer pr buss"
      , setter =
            (\value state ->
                { state
                    | passasjererPerBuss = value
                }
            )
      , accessor = .passasjererPerBuss
      , stepSize = 5
      }
    , { name = "yearlyTidsbesparelseMinutter"
      , title = "Årlig tidsbesparelse ved raskere på- og avstigning, minutter"
      , placeholder = "Se forklarende tekst i rapport"
      , setter =
            (\value state ->
                { state
                    | yearlyTidsbesparelseMinutter = value
                }
            )
      , accessor = .yearlyTidsbesparelseMinutter
      , stepSize = 1000
      }
    , { name = "passasjererTilpassedeHoldplasserPercent"
      , title = "Andel passasjerer som benytter tilpassede holdeplasser"
      , placeholder = "Prosent av passasjerene"
      , setter =
            (\value state ->
                { state
                    | passasjererTilpassedeHoldplasserPercent = value
                }
            )
      , accessor = .passasjererTilpassedeHoldplasserPercent
      , stepSize = 1
      }
    ]


fields : List Field
fields =
    let
        stateMap updater tiltakStates =
            { tiltakStates
                | laventrebuss = updater tiltakStates.laventrebuss
            }

        updateTiltakStateHelper =
            TiltakStates.stateUpdateHelper stateMap

        thisValueHelper =
            TiltakStates.valueHelper .laventrebuss
    in
        fieldDefinitions
            |> Field.transformToFields
                stateMap
                updateTiltakStateHelper
                thisValueHelper
