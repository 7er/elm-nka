module Tiltak.OpphoeyetHoldeplass exposing (..)

import Tiltak exposing (Tiltak(..), GraphState(..), Field, SimpleField, StateCalculationMethod, sendTo)
import TiltakStates exposing (TiltakStates, OpphoyetHoldeplassState)
import Tiltak.BasicTiltak as BasicTiltak
import GeneralForutsetninger


yearlyPassasjerNytte : StateCalculationMethod
yearlyPassasjerNytte this ({ opphoeyetHoldeplass } as state) =
    let
        verdisettinger =
            GeneralForutsetninger.verdisettinger

        firstCalc passengersPerYear =
            passengersPerYear * verdisettinger.opphoyetHoldeplass

        first =
            Maybe.map firstCalc opphoeyetHoldeplass.passengersPerYear

        secondCalc beleggForbiPassasjererPerBuss aarligTidsbesparelseMinutter =
            beleggForbiPassasjererPerBuss
                * aarligTidsbesparelseMinutter
                * verdisettinger.reisetidKollektivTransport

        second =
            Maybe.map2
                secondCalc
                opphoeyetHoldeplass.beleggForbiPassasjererPerBuss
                opphoeyetHoldeplass.aarligTidsbesparelseMinutter
    in
        Maybe.map2 (+) first second


yearlyOperatoerNytte : StateCalculationMethod
yearlyOperatoerNytte this ({ opphoeyetHoldeplass } as state) =
    let
        verdisettinger =
            GeneralForutsetninger.verdisettinger
    in
        Maybe.map (\minutter -> minutter * verdisettinger.operatoerKostnad) opphoeyetHoldeplass.aarligTidsbesparelseMinutter


levetid : number
levetid =
    25


graphState : Tiltak -> TiltakStates -> GraphState
graphState this { opphoeyetHoldeplass } =
    opphoeyetHoldeplass.installationCost
        |> Maybe.map (\_ -> GraphOn)
        |> Maybe.withDefault GraphOff


findVariableToGraph : Tiltak -> TiltakStates -> Field
findVariableToGraph this ({ opphoeyetHoldeplass } as state) =
    let
        maybeField =
            sendTo this .fields |> List.head
    in
        case maybeField of
            Just value ->
                value

            Nothing ->
                Debug.crash "TODO"


nettoNytteNullpunktFor : Tiltak -> TiltakStates -> Field -> Float
nettoNytteNullpunktFor tiltak state field =
    case field.name of
        "installationCost" ->
            1.0e6

        _ ->
            Debug.crash (toString field)


samples : Tiltak -> TiltakStates -> Field -> List Float
samples this state field =
    let
        samplesOnEachSide =
            5

        minimum =
            0

        nullPunkt =
            nettoNytteNullpunktFor this state field

        start =
            max (nullPunkt - (field.stepSize * samplesOnEachSide)) minimum
    in
        List.range 0 (samplesOnEachSide * 2)
            |> List.map toFloat
            |> List.map (\index -> start + index * field.stepSize)


graphData : Tiltak -> TiltakStates -> List ( Float, Float )
graphData this ({ opphoeyetHoldeplass } as state) =
    let
        field =
            findVariableToGraph this state

        generateData x =
            let
                newState =
                    field.updateValue x state
            in
                sendTo this .nettoNytte newState |> Maybe.map (\y -> ( x, y ))
    in
        samples this state field
            |> List.map generateData
            |> List.filterMap identity


tiltak : Tiltak
tiltak =
    let
        basicTiltakRecord =
            BasicTiltak.basicTiltakRecord
    in
        Tiltak
            { basicTiltakRecord
                | title = \_ -> "Opphøyet holdeplass"
                , fields = \_ -> fields
                , yearlyPassasjerNytte = yearlyPassasjerNytte
                , yearlyOperatoerNytte = yearlyOperatoerNytte
                , investeringsKostInklRestverdi =
                    \_ { opphoeyetHoldeplass } ->
                        BasicTiltak.investeringsKostInklRestverdi
                            opphoeyetHoldeplass
                            levetid
                , driftOgVedlihKost =
                    \_ { opphoeyetHoldeplass } ->
                        BasicTiltak.driftOgVedlihKost opphoeyetHoldeplass
                , skyggepris =
                    \this ({ opphoeyetHoldeplass } as state) ->
                        sendTo
                            this
                            .skyggeprisHelper
                            state
                            opphoeyetHoldeplass.bompengeAndel
                , graphState = graphState
                , graphData = graphData
            }


initialState : OpphoyetHoldeplassState
initialState =
    { installationCost = Nothing
    , yearlyMaintenance = Nothing
    , bompengeAndel = 0
    , passengersPerYear = Nothing
    , beleggForbiPassasjererPerBuss = Nothing
    , aarligTidsbesparelseMinutter = Nothing
    }


fieldDefinitions : List (SimpleField OpphoyetHoldeplassState)
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
      , title = "Antall passasjerer på holdeplassen"
      , placeholder = "På- og avstigende passasjerer per år"
      , setter =
            (\value state ->
                { state
                    | passengersPerYear = value
                }
            )
      , accessor = .passengersPerYear
      , stepSize = 50
      }
    , { name = "beleggForbiPassasjererPerBuss"
      , title = "Gjennomstsbelegg forbi holdeplassen"
      , placeholder = "Passasjerer pr buss"
      , setter =
            (\value state ->
                { state
                    | beleggForbiPassasjererPerBuss = value
                }
            )
      , accessor = .beleggForbiPassasjererPerBuss
      , stepSize = 50
      }
    , { name = "aarligTidsbesparelseMinutter"
      , title = "Årlig tidsbesparelse i minutter"
      , placeholder = "Se tekst i rapport for detaljer"
      , setter =
            (\value state ->
                { state
                    | aarligTidsbesparelseMinutter = value
                }
            )
      , accessor = .aarligTidsbesparelseMinutter
      , stepSize = 1000
      }
    ]


fields : List Field
fields =
    let
        stateMap updater tiltakStates =
            { tiltakStates
                | opphoeyetHoldeplass = updater tiltakStates.opphoeyetHoldeplass
            }

        updateTiltakStateHelper =
            TiltakStates.stateUpdateHelper stateMap

        thisStringValueHelper =
            TiltakStates.stringValueHelper .opphoeyetHoldeplass
    in
        fieldDefinitions
            |> Tiltak.transformToFields
                stateMap
                updateTiltakStateHelper
                thisStringValueHelper
