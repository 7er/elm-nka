module Assets exposing (Image(..), byvaapen, toiLogo, trikkRikshospitalet, src)

{-| Assets, such as images, videos, and audio. (We only have images for now.)
We should never expose asset URLs directly; this module should be in charge of
all of them. One source of truth!
-}

import Html exposing (Attribute, Html)
import Html.Attributes as Attr


type Image
    = Image String



-- IMAGES --


byvaapen : Image
byvaapen =
    Image "./images/oslo_byvaapen_liten.png"


toiLogo : Image
toiLogo =
    Image "./images/toi_logo_navn.png"


trikkRikshospitalet : Image
trikkRikshospitalet =
    Image "./images/trikk_rikshospitalet.jpg"

-- USING IMAGES --


src : Image -> Attribute msg
src (Image url) =
    Attr.src url
