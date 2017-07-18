module Main exposing (..)

import Html exposing (Html, div, text, h2)


view : Model -> Html msg
view model =
    let
        tiltakDiv tiltak =
            div [] <| page tiltak model
    in
        div [] (tiltakene |> List.map tiltakDiv)


form tiltak model =
    Form.form [ onSubmit (FormSubmit handleSubmit) ]
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
        )


page : Tiltak -> Model -> List (Html msg)
page tiltak model =
    [ h2 [] [ text tiltak.title ]
    , form tiltak model
    , div [] [ text ("Tiltaket som kalkulerer " ++ (toString (tiltak.calculation model.tiltakStates))) ]
    ]
