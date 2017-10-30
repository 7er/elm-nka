module Assets exposing (..)

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


backArrow : Image
backArrow =
    Image "./images/689246_arrows_512x512.png"


trikkRikshospitalet : Image
trikkRikshospitalet =
    Image "./images/trikk_rikshospitalet.jpg"


bussholdeplass : Image
bussholdeplass =
    Image "./images/samuel-foster-199587.jpg"


caretRight : Image
caretRight =
    Image "./images/caret_right.gif"



-- Group icons


holdeplasser =
    Image
        "./images/holdeplass_noun_1230795_cc.gif"


informasjon =
    Image
        "./images/info_noun_45.gif"


trygghet =
    Image "./images/sikkerhet.gif"


kjoeremateriell =
    Image
        "./images/buss_noun_1308908_cc.gif"


strekningOgFramkommelighet =
    Image "./images/gatekryss_noun_1277963_cc.gif"


tilgjengelighet =
    Image "./images/noun_1174765_cc.png"



-- USING IMAGES --


src : Image -> Attribute msg
src (Image url) =
    Attr.src url
