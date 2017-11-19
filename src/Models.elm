port module Models exposing (..)

import Bootstrap.Accordion as Accordion
import Focus exposing (Focus)
import TiltakStates exposing (TiltakStates)
import FormattedValue exposing (FormattedValue)


type Group
    = Holdeplasser
    | Informasjon
    | Trygghet
    | Kjoeremateriell
    | StrekningOgFramkommelighet
    | Tilgjengelighet


type Page
    = Home
    | GroupPage Group
    | NotFound


type alias Model =
    { page : Page
    , accordionState : Accordion.State
    , tiltakStates : TiltakStates
    , chartIds : List String
    }


type alias Field =
    { name : String
    , title : String
    , placeholder : String

    --    , updateValue : Float -> TiltakStates -> TiltakStates
    , stepSize : Float
    , value : TiltakStates -> Maybe Float
    , isEditable : TiltakStates -> Bool
    , beDisplayMode : TiltakStates -> TiltakStates
    , beEditMode : TiltakStates -> TiltakStates
    , focus : Focus TiltakStates (FormattedValue Float)
    }
