module SimpleTiltak exposing (..)

import TiltakStates exposing (TiltakStates, SimpleCommonState)
import Field exposing (SimpleField)
import BasicTiltak
import Tiltak exposing (Tiltak(..), StateCalculationMethod, bindTiltak, sendTo)


{-
   type alias SimpleTiltak =
   {
       stateMap : (SimpleCommonState -> SimpleCommonState) -> TiltakStates -> TiltakStates
   , getter : SimpleCommon

           getter =
               .belysning

           nytteMultiplikator =
               verdisettinger.belysning

           levetid =
               20

           title =
               "Belysning på holdeplass"

       }
-}


initialState : TiltakStates.SimpleCommonState
initialState =
    { installationCost = Nothing
    , yearlyMaintenance = Nothing
    , passengersPerYear = Nothing
    , bompengeAndel = 0
    }


fieldDefinitions : List (SimpleField TiltakStates.SimpleCommonState)
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
      , title = "Antall passasjerer per år"
      , placeholder = "Påstigende passasjerer per år"
      , setter =
            (\value state ->
                { state
                    | passengersPerYear =
                        value
                }
            )
      , accessor = .passengersPerYear
      , stepSize = 50
      }
    ]



--createTiltak : SimpleTiltak -> Tiltak


createTiltak simpleTiltak =
    let
        basicTiltakRecord =
            BasicTiltak.basicTiltakRecord
    in
        Tiltak
            { basicTiltakRecord
                | title = \_ -> simpleTiltak.title
                , fields =
                    \_ ->
                        Field.compileFields simpleTiltak.stateMap
                            simpleTiltak.getter
                            fieldDefinitions
                , skyggepris =
                    \this state ->
                        sendTo this .skyggeprisHelper state ((simpleTiltak.getter state).bompengeAndel)
                , yearlyPassasjerNytte =
                    \_ state ->
                        (simpleTiltak.getter state).passengersPerYear
                            |> Maybe.map ((*) simpleTiltak.nytteMultiplikator)
                , driftOgVedlihKost =
                    \_ state ->
                        BasicTiltak.driftOgVedlihKost (simpleTiltak.getter state)
                , investeringsKostInklRestverdi =
                    \_ state ->
                        BasicTiltak.investeringsKostInklRestverdi
                            (simpleTiltak.getter state)
                            simpleTiltak.levetid
            }
