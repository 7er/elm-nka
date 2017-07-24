module Group exposing (..)

import Models exposing (Group(..))


groupPath : Group -> String
groupPath tag =
    tag |> groupPathSansHash |> (++) "#"


groupPathSansHash : Group -> String
groupPathSansHash tag =
    tag |> toString |> String.toLower


groupTitle : Group -> String
groupTitle tag =
    case tag of
        StrekningOgFramkommelighet ->
            "Strekning og framkommelighet"

        _ ->
            tag |> toString
