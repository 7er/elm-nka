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


toggleTiltak : Model -> Tiltak -> Model
toggleTiltak model tiltaket =
    let
        toggleVisible tiltak =
            case tiltaket == tiltak of
                True ->
                    { tiltak | visible = not tiltak.visible }

                False ->
                    { tiltak | visible = False }

        maybeTiltaksGruppe =
            case model.route of
                GruppeRoute tiltaksGruppa ->
                    Just { tiltaksGruppa | tiltakene = List.map toggleVisible tiltaksGruppa.tiltakene }

                NotFoundRoute ->
                    Nothing

                Root ->
                    Nothing
    in
        case maybeTiltaksGruppe of
            Just tiltaksGruppe ->
                { model | route = GruppeRoute tiltaksGruppe }

            Nothing ->
                model


updateData : Model -> Tiltak -> String -> Model
updateData model tiltaket newData =
    let
        update tiltak =
            case tiltaket == tiltak of
                True ->
                    { tiltak | data = newData }

                False ->
                    tiltak

        maybeTiltaksGruppe =
            case model.route of
                GruppeRoute tiltaksGruppa ->
                    Just { tiltaksGruppa | tiltakene = List.map update tiltaksGruppa.tiltakene }

                NotFoundRoute ->
                    Nothing

                Root ->
                    Nothing
    in
        case maybeTiltaksGruppe of
            Just tiltaksGruppe ->
                { model | route = GruppeRoute tiltaksGruppe }

            Nothing ->
                model


tiltaksGruppePath : TiltaksGruppe -> String
tiltaksGruppePath { tag } =
    tag |> toString |> String.toLower |> (++) "#"


tiltaksGruppeTittel : TiltaksGruppe -> String
tiltaksGruppeTittel { tag } =
    tag |> toString
