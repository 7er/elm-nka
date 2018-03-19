module Tiltak.Laventrebuss exposing (..)

import Focus exposing ((=>))
import Tiltak exposing (Tiltak(..), StateCalculationMethod, sendTo)
import Field exposing (Field, SimpleField)
import SpecificStates exposing (LaventrebussState)
import FormattedValue
    exposing
        ( formattedValueDefault
        , installationCost
        , yearlyMaintenance
        , value
        , passengersPerYear
        )
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
                laventrebuss.passengersPerYear.value
                laventrebuss.passasjererTilpassedeHoldplasserPercent.value

        secondCalc =
            Maybe.map2
                (\passengersPerYear passasjererTilpassedeHoldplasserPercent ->
                    passengersPerYear
                        * (1 - passasjererTilpassedeHoldplasserPercent)
                        * verdisettinger.lavgulvUtenTilpassetHoldeplass
                )
                laventrebuss.passengersPerYear.value
                laventrebuss.passasjererTilpassedeHoldplasserPercent.value

        thirdCalc =
            Maybe.map2
                (\passasjererPerBuss yearlyTidsbesparelseMinutter ->
                    passasjererPerBuss
                        * yearlyTidsbesparelseMinutter
                        * verdisettinger.reisetidKollektivTransport
                )
                laventrebuss.passasjererPerBuss.value
                laventrebuss.yearlyTidsbesparelseMinutter.value
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
        laventrebuss.yearlyTidsbesparelseMinutter.value


levetid : number
levetid =
    10


tiltak : Tiltak
tiltak =
    let
        basicTiltakRecord =
            BasicTiltak.basicTiltakRecord specificState
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
    { installationCost = formattedValueDefault
    , yearlyMaintenance = formattedValueDefault
    , bompengeAndel = 0
    , passengersPerYear = formattedValueDefault
    , passasjererPerBuss = formattedValueDefault
    , yearlyTidsbesparelseMinutter = formattedValueDefault
    , passasjererTilpassedeHoldplasserPercent = formattedValueDefault
    , preferredToGraph = ""
    }


fieldDefinitions : List SimpleField
fieldDefinitions =
    let
        passasjererPerBuss =
            Focus.create
                .passasjererPerBuss
                (\f specificState ->
                    { specificState
                        | passasjererPerBuss = f specificState.passasjererPerBuss
                    }
                )

        yearlyTidsbesparelseMinutter =
            Focus.create
                .yearlyTidsbesparelseMinutter
                (\f specificState ->
                    { specificState
                        | yearlyTidsbesparelseMinutter =
                            f specificState.yearlyTidsbesparelseMinutter
                    }
                )

        passasjererTilpassedeHoldplasserPercent =
            Focus.create
                .passasjererTilpassedeHoldplasserPercent
                (\f specificState ->
                    { specificState
                        | passasjererTilpassedeHoldplasserPercent =
                            f specificState.passasjererTilpassedeHoldplasserPercent
                    }
                )
    in
        [ { name = "installationCost"
          , title = "Installasjonskostnad"
          , placeholder = "Kostnaden ved å installere tiltaket en gang, kroner"
          , focus = specificState => installationCost
          , stepSize = 50000
          }
        , { name = "yearlyMaintenance"
          , title = "Årlige drifts- og vedlikeholdskostnader"
          , placeholder = BasicTiltak.yearlyMaintenancePlaceholder
          , focus = specificState => yearlyMaintenance
          , stepSize = 5000
          }
        , { name = "passengersPerYear"
          , title = "Antall passasjerer på hele ruta per år"
          , placeholder = "Passasjerer per år"
          , focus = specificState => passengersPerYear
          , stepSize = 50
          }
        , { name = "passasjererPerBuss"
          , title = "Gjennomsnittsbelegg på bussene"
          , placeholder = "Passasjerer pr buss"
          , focus = specificState => passengersPerYear
          , stepSize = 5
          }
        , { name = "yearlyTidsbesparelseMinutter"
          , title = "Årlig tidsbesparelse"
          , placeholder = "Tidsbesparelse ved raskere på- og avstigning, minutter"
          , focus = specificState => yearlyTidsbesparelseMinutter
          , stepSize = 1000
          }
        , { name = "passasjererTilpassedeHoldplasserPercent"
          , title = "Andel passasjerer som benytter tilpassede holdeplasser"
          , placeholder = "Prosent av passasjerene"
          , focus = specificState => passasjererTilpassedeHoldplasserPercent
          , stepSize = 1
          }
        ]


specificState : Focus.Focus { b | laventrebuss : a } a
specificState =
    Focus.create
        .laventrebuss
        (\f tiltakStates ->
            { tiltakStates
                | laventrebuss = f tiltakStates.laventrebuss
            }
        )


fields : List Field
fields =
    fieldDefinitions
        |> Field.transformToFields
