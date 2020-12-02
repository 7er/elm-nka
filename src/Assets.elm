module Assets exposing (..)

{-| Assets, such as images, videos, and audio. (We only have images for now.)
We should never expose asset URLs directly; this module should be in charge of
all of them. One source of truth!
-}

import Html exposing (Attribute, Html)
import Html.Attributes as Attr


type Path
    = Path String



-- DOCUMENTS --


arbeidsdokument51690 =
    Path "./docs/arbeidsdok-51690-kollektivkalkulator-2020.pdf"



-- IMAGES --


byvaapen =
    Path "./images/oslo_byvaapen_liten.png"


toiLogo =
    Path "./images/toi_logo_navn.png"


backArrow =
    Path "./images/689246_arrows_512x512.png"


trikkRikshospitalet =
    Path "./images/trikk_rikshospitalet.jpg"


bussholdeplass =
    Path "./images/samuel-foster-199587.jpg"


caretRight =
    Path "./images/caret_right.gif"



-- Group icons


holdeplasser =
    Path
        "./images/holdeplass_noun_1230795_cc.gif"


informasjon =
    Path
        "./images/info_noun_45.gif"


trygghet =
    Path "./images/sikkerhet.gif"


kjoeremateriell =
    Path
        "./images/buss_noun_1308908_cc.gif"


strekningOgFramkommelighet =
    Path "./images/gatekryss_noun_1277963_cc.gif"


tilgjengelighet =
    Path "./images/noun_1050600_cc.gif"



-- USING IMAGES --


src : Path -> Attribute msg
src (Path url) =
    Attr.src url


href : Path -> Attribute msg
href (Path url) =
    Attr.href url
