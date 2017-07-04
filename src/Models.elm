module Models exposing (..)


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


type Route
    = Root
    | GruppeRoute TiltaksGruppe
    | NotFoundRoute


type alias Model =
    { message : String
    , printMediaType : Bool
    , tiltaksGrupper : List TiltaksGruppe
    , route : Route
    }


createTiltak : String -> String -> Tiltak
createTiltak name data =
    { name = name
    , data = data
    , visible = False
    }


toggleTiltak : TiltaksGruppe -> Tiltak -> TiltaksGruppe
toggleTiltak gruppe tiltaket =
    let
        toggleVisible tiltak =
            case tiltaket == tiltak of
                True ->
                    { tiltak | visible = not tiltak.visible }

                False ->
                    { tiltak | visible = False }
    in
        { gruppe | tiltakene = List.map toggleVisible gruppe.tiltakene }


updateData : TiltaksGruppe -> Tiltak -> String -> TiltaksGruppe
updateData gruppe tiltaket newData =
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
