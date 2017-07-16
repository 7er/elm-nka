module TiltakPage exposing (..)

import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Button as Button
import Html exposing (Html, text, div, h2)
import Html.Attributes exposing (for, value, id, class)
import Html.Events exposing (onSubmit)
import Field exposing (..)
import Models exposing (Model, TiltakStates)
import Msgs exposing (Msg(..), SubmitFunc)


updateTiltakState :
    TiltakState a
    -> String
    -> FieldValue
    -> List (Field a)
    -> TiltakState a
updateTiltakState formState variableName stringValue fields =
    case Field.findField variableName fields of
        Just { storeFunc } ->
            { formState | specificState = storeFunc (unwrapState formState) stringValue }

        _ ->
            Debug.crash "TODO"


form :
    SubmitFunc
    -> (String -> FieldValue -> TiltakStates -> TiltakStates)
    -> List (Field a)
    -> Model
    -> List (Html Msg)
form handleSubmit updateFieldInModel fields model =
    [ Form.form [ onSubmit (FormSubmit handleSubmit) ]
        (List.append
            (fields
                |> List.map
                    (\{ name, title, placeholder } ->
                        Form.group []
                            [ Form.label [ for name ] [ text title ]
                            , Input.number
                                [ Input.id name
                                , Input.placeholder placeholder
                                , Input.onInput (FieldUpdate (\stringValue tiltakStates -> updateFieldInModel name stringValue tiltakStates))
                                ]
                            ]
                    )
            )
            [ Form.group []
                [ Form.label [ for "variableToGraph" ] [ text "Velg verdi som skal vises på X-aksen i grafen" ]
                , fields
                    |> List.map (\{ name, title } -> Select.item [ value name ] [ text title ])
                    |> Select.select [ Select.id "variableToGraph" ]
                ]
            , Button.button [ Button.primary ] [ text "Submit" ]
            ]
        )
    ]
