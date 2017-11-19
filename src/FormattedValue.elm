module FormattedValue exposing (..)

import Focus exposing (Focus, (=>))


type Editable
    = Edit
    | Display


type alias FormattedValue valueType =
    { value : Maybe valueType
    , state : Editable
    }


state =
    Focus.create
        .state
        (\f formattedValue ->
            { formattedValue | state = f formattedValue.state }
        )


formattedValueDefault : FormattedValue a
formattedValueDefault =
    { value = Nothing
    , state = Display
    }


formattedValue value =
    { value = value
    , state = Display
    }


value : Focus { formattedValue | value : Maybe a } (Maybe a)
value =
    Focus.create
        .value
        (\f formattedValue ->
            { formattedValue | value = f formattedValue.value }
        )


yearlyMaintenance : Focus { specificState | yearlyMaintenance : a } a
yearlyMaintenance =
    Focus.create
        .yearlyMaintenance
        (\f specificState ->
            { specificState | yearlyMaintenance = f specificState.yearlyMaintenance }
        )


passengersPerYear : Focus { specificState | passengersPerYear : a } a
passengersPerYear =
    Focus.create
        .passengersPerYear
        (\f specificState ->
            { specificState | passengersPerYear = f specificState.passengersPerYear }
        )


installationCost : Focus { specificState | installationCost : a } a
installationCost =
    Focus.create
        .installationCost
        (\f specificState ->
            { specificState | installationCost = f specificState.installationCost }
        )


bompengeAndel =
    Focus.create
        .bompengeAndel
        (\f specificState ->
            { specificState | bompengeAndel = f specificState.bompengeAndel }
        )


installationCostValue =
    installationCost => value
