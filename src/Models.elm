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
    = GruppeSide TiltaksGruppe


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


toggleTiltak : Model -> Tiltak -> Model
toggleTiltak model tiltaket =
    let
        toggleVisible tiltak =
            case tiltaket == tiltak of
                True ->
                    { tiltak | visible = not tiltak.visible }

                False ->
                    { tiltak | visible = False }

        tiltaksGruppe =
            case model.route of
                GruppeSide tiltaksGruppa ->
                    { tiltaksGruppa | tiltakene = List.map toggleVisible tiltaksGruppa.tiltakene }
    in
        { model | route = GruppeSide tiltaksGruppe }


updateData : Model -> Tiltak -> String -> Model
updateData model tiltaket newData =
    let
        update tiltak =
            case tiltaket == tiltak of
                True ->
                    { tiltak | data = newData }

                False ->
                    tiltak

        tiltaksGruppe =
            case model.route of
                GruppeSide tiltaksGruppa ->
                    { tiltaksGruppa | tiltakene = List.map update tiltaksGruppa.tiltakene }
    in
        { model | route = GruppeSide tiltaksGruppe }
