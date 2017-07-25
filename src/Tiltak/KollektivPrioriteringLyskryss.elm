module Tiltak.KollektivPrioriteringLyskryss exposing (..)

import Tiltak exposing (Tiltak(..), sendTo, StateCalculationMethod, Field)
import Tiltak.BasicTiltak as BasicTiltak
import TiltakStates exposing (KollektivPrioriteringLyskryssState)
import GeneralForutsetninger


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
                    * GeneralForutsetninger.reisetidKollektivTransportNOK
                    * passengersPerYear
            )


yearlyTrafikantNytte : StateCalculationMethod
yearlyTrafikantNytte this ({ kollektivPrioriteringLyskryss } as state) =
    Maybe.map3
        (\antallBilerForsinketPerAvgang antallPasserendeAvgangerPerYear forsinkelsePerBilSeconds ->
            antallBilerForsinketPerAvgang
                * antallPasserendeAvgangerPerYear
                * (negate forsinkelsePerBilSeconds / 60)
                * GeneralForutsetninger.reisetidBilNOK
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
                * GeneralForutsetninger.operatoerKostnadNOK
    in
        kollektivPrioriteringLyskryss.antallPasserendeAvgangerPerYear
            |> Maybe.map calculation


investeringsKostInklRestverdi : StateCalculationMethod
investeringsKostInklRestverdi this ({ kollektivPrioriteringLyskryss } as state) =
    kollektivPrioriteringLyskryss.installationCost
        |> Maybe.map
            (\installationCost -> installationCost * GeneralForutsetninger.investeringsFaktor levetid)
        |> Maybe.map negate


driftOgVedlihKost : StateCalculationMethod
driftOgVedlihKost this ({ kollektivPrioriteringLyskryss } as state) =
    kollektivPrioriteringLyskryss.yearlyMaintenance
        |> Maybe.map ((*) GeneralForutsetninger.afaktor)
        |> Maybe.map negate


skyggepris : StateCalculationMethod
skyggepris this ({ kollektivPrioriteringLyskryss } as state) =
    let
        calculation kostUtenSkyggepris =
            (1 - kollektivPrioriteringLyskryss.bompengeAndel)
                * kostUtenSkyggepris
                * GeneralForutsetninger.skyggepris
    in
        (sendTo this .kostUtenSkyggepris state)
            |> Maybe.map calculation


tiltak : Tiltak
tiltak =
    let
        basicTiltakRecord =
            BasicTiltak.basicTiltakRecord
    in
        Tiltak
            { basicTiltakRecord
                | title = \_ -> "Kollektiv prioritering lyskryss"
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
    }


type alias SimpleField =
    { name : String
    , title : String
    , placeholder : String
    , setter :
        Maybe Float
        -> KollektivPrioriteringLyskryssState
        -> KollektivPrioriteringLyskryssState
    , accessor : KollektivPrioriteringLyskryssState -> Maybe Float
    }


fieldDefinitions : List SimpleField
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
      }
    , { name = "passengersPerYear"
      , title = "Antall passasjerer per år"
      , placeholder = "Påstigende passasjerer per år"
      , setter =
            (\value state ->
                { state
                    | passengersPerYear = value
                }
            )
      , accessor = .passengersPerYear
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

        thisStringValueHelper =
            TiltakStates.stringValueHelper .kollektivPrioriteringLyskryss

        toRealField simpleField =
            { name = simpleField.name
            , title = simpleField.title
            , placeholder = simpleField.placeholder
            , updateTiltakState =
                updateTiltakStateHelper
                    (\stringValue state ->
                        let
                            pipeline =
                                String.toFloat stringValue
                                    |> Result.toMaybe
                                    |> simpleField.setter
                        in
                            pipeline state
                    )
            , stringValueFromState = thisStringValueHelper simpleField.accessor
            }
    in
        fieldDefinitions
            |> List.map toRealField
