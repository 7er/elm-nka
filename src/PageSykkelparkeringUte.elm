module PageSykkelparkeringUte exposing (..)

import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Button as Button
import Html exposing (Html, text)
import Html.Attributes exposing (for, value)
import Html.Events exposing (onSubmit)
import ModelAndMsg exposing (..)


type alias Title =
    String


initialFormState : SykkelparkeringUteFormState
initialFormState =
    { tripsPerYear = Nothing }


updateFormState : SykkelparkeringUteFormState -> VariableName -> String -> SykkelparkeringUteFormState
updateFormState formState variableName stringValue =
    case variableName of
        "tripsPerYear" ->
            -- maybeValue : Maybe Int
            let maybeValue = case String.toInt stringValue of
                Ok value -> Just value
                Err message -> Debug.log message Nothing
            in
                { formState | tripsPerYear = maybeValue  }
        _ ->
            Debug.crash "TODO"




--variableNameAndTitle : (List (VariableName, Title))


variableNameAndTitle =
    [ ( "tripsPerYear", "Antall sykkelreiser per år" )
    , ( "yearlyMaintenance", "Årlige drifts- og vedlikeholdskostnader" )
    , ( "installationCost", "Installasjonskostnad" )
    ]


page : Model -> List (Html Msg)
page model =
    [ Form.form [ onSubmit SykkelparkeringUteSubmit ]
        [ Form.group []
            [ Form.label [ for "tripsPerYear" ] [ text "Antall sykkelreiser per år" ]
            , Input.number
                [ Input.id "tripsPerYear"
                , Input.placeholder "Sykkelreiser som bruker tiltaket"
                , Input.onInput (SykkelparkeringUteForm "tripsPerYear")
                ]
            ]
        , Form.group []
            [ Form.label [ for "installationCost" ] [ text "Installasjonskostnad" ]
            , Input.number [ Input.id "installationCost" ]
            ]
        , Form.group []
            [ Form.label [ for "yearlyMaintenance" ] [ text "Årlige drifts- og vedlikeholdskostnader" ]
            , Input.number [ Input.id "yearlyMaintenance" ]
            ]
        , Form.group []
            [ Form.label [ for "variableToGraph" ] [ text "Velg verdi som skal vises på X-aksen i grafen" ]
            , Select.select [ Select.id "variableToGraph" ]
                (variableNameAndTitle
                    |> List.map (\( name, title ) -> Select.item [ value name ] [ text title ])
                )
            ]
        , Button.button [ Button.primary ] [ text "Submit" ]
        ]
    ]
