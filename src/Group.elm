module Group exposing (..)

import Models exposing (Group)


groupPath : Group -> String
groupPath tag =
    tag |> groupPathSansHash |> (++) "#"


groupPathSansHash : Group -> String
groupPathSansHash tag =
    tag |> groupTitle |> String.toLower


groupTitle : Group -> String
groupTitle tag =
    tag |> toString
