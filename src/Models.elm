port module Models exposing (..)

import Navigation exposing (Location)
import Bootstrap.Navbar as Navbar
import Bootstrap.Modal as Modal
import SykkelparkeringUteTiltak exposing (SykkelparkeringUteTiltakModel)
import SeparatSykkelvegTiltak exposing (SeparatSykkelvegTiltakModel)
import Field exposing (..)


port generateC3 : String -> Cmd msg


type Page
    = Home
    | GettingStarted
    | SykkelparkeringUte
    | SeparatSykkelveg
    | GroupPage TiltaksGruppeType
    | NotFound


type alias Model =
    { page : Page
    , navState : Navbar.State
    , modalState : Modal.State
    , sykkelParkeringUteFormState : FormState SykkelparkeringUteTiltakModel
    , separatSykkelvegFormState : FormState SeparatSykkelvegTiltakModel
    }


type alias UpdateFunc =
    FieldValue -> Model -> Model


type alias SubmitFunc =
    Model -> ( Model, Cmd Msg )


type Msg
    = UrlChange Location
    | NavMsg Navbar.State
    | ModalMsg Modal.State
    | FieldUpdate UpdateFunc String
    | FormSubmit SubmitFunc


type TiltaksGruppeType
    = Holdeplasser
    | Informasjon


type alias Tiltak =
    { name : String
    , data : String
    , visible : Bool
    }


type alias TiltaksGruppe =
    { tag : TiltaksGruppeType
    , tiltakene : List Tiltak
    }


createTiltak : String -> String -> Tiltak
createTiltak name data =
    { name = name
    , data = data
    , visible = False
    }


tiltaksGrupper : List TiltaksGruppe
tiltaksGrupper =
    [ { tag = Holdeplasser
      , tiltakene =
            [ createTiltak "Sykkelparkering" "Foobar"
            , createTiltak "Leskur u sitteplass" "Zppt"
            , createTiltak "Sitteplass på hpl" "Syver"
            ]
      }
    , { tag = Informasjon
      , tiltakene =
            [ createTiltak "Skilting i buss" "Foobar"
            , createTiltak "Hpl. opprop" "Zppt"
            ]
      }
    ]


toggleTiltak : Tiltak -> TiltaksGruppe -> TiltaksGruppe
toggleTiltak tiltaket gruppe =
    let
        toggleVisible tiltak =
            case tiltaket == tiltak of
                True ->
                    { tiltak | visible = not tiltak.visible }

                False ->
                    { tiltak | visible = False }
    in
        { gruppe | tiltakene = List.map toggleVisible gruppe.tiltakene }


updateData : Tiltak -> String -> TiltaksGruppe -> TiltaksGruppe
updateData tiltaket newData gruppe =
    let
        update tiltak =
            case tiltaket == tiltak of
                True ->
                    { tiltak | data = newData }

                False ->
                    tiltak
    in
        { gruppe | tiltakene = List.map update gruppe.tiltakene }


tiltaksGruppePath : TiltaksGruppe -> String
tiltaksGruppePath { tag } =
    tag |> toString |> String.toLower |> (++) "#"


tiltaksGruppeTittel : TiltaksGruppe -> String
tiltaksGruppeTittel { tag } =
    tag |> toString


activeGruppe : Model -> TiltaksGruppeType -> Maybe TiltaksGruppe
activeGruppe model activeTag =
    let
        filter { tag } =
            tag == activeTag
    in
        List.head (List.filter filter tiltaksGrupper)


gruppeFromHash : String -> Maybe TiltaksGruppeType
gruppeFromHash hash =
    let
        filter gruppe =
            hash == tiltaksGruppePath gruppe
    in
        case List.head (List.filter filter tiltaksGrupper) of
            Just gruppe ->
                Just gruppe.tag

            Nothing ->
                Nothing
