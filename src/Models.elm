module Models exposing (..)

import Navigation exposing (Location)


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
    | GruppeRoute TiltaksGruppeType
    | NotFoundRoute


type alias Model =
    { tiltaksGrupper : List TiltaksGruppe
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


gruppeFromHash : Model -> String -> Maybe TiltaksGruppeType
gruppeFromHash model hash =
    let
        filter gruppe =
            hash == tiltaksGruppePath gruppe
    in
        case List.head (List.filter filter model.tiltaksGrupper) of
            Just gruppe ->
                Just gruppe.tag

            Nothing ->
                Nothing


routeFromLocation : Model -> Location -> Route
routeFromLocation model location =
    case gruppeFromHash model location.hash of
        Just tag ->
            GruppeRoute tag

        Nothing ->
            case location.hash of
                "" ->
                    Root

                _ ->
                    NotFoundRoute
