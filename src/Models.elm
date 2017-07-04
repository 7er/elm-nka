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
        List.head (List.filter filter model.tiltaksGrupper)
