module TiltakPage exposing (..)

import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Button as Button
import Html exposing (Html, text, div, h2)
import Html.Attributes exposing (for, value, id, class)
import Html.Events exposing (onSubmit)
import Field exposing (..)
import Models exposing (Msg(..), Model)


updateFormState :
    FormState a
    -> String
    -> FieldValue
    -> List (Field a)
    -> FormState a
updateFormState formState variableName stringValue fields =
    case Field.findField variableName fields of
        Just { storeFunc } ->
            storeFunc formState stringValue

        _ ->
            Debug.crash "TODO"


form :
    Models.SubmitFunc
    -> (String -> FieldValue -> Model -> Model)
    -> List { a | placeholder : String, name : String, title : String }
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
                                , Input.onInput (FieldUpdate (\stringValue model -> updateFieldInModel name stringValue model))
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
