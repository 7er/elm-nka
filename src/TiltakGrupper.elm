module TiltakGrupper exposing (..)

import Models exposing (..)
import Msgs exposing (TiltakObject)
import TiltakComponents.SykkelparkeringUte as SykkelparkeringUte
import TiltakComponents.SeparatSykkelveg as SeparatSykkelveg
import TiltakComponents.LeskurUtenSitteplass as LeskurUtenSitteplass
import TiltakComponents.SkiltingIBuss as SkiltingIBuss


type alias TiltaksGruppe =
    { tag : TiltaksGruppeType
    , tiltakene : List TiltakObject
    }


tiltaksGrupper : List TiltaksGruppe
tiltaksGrupper =
    [ { tag = Holdeplasser
      , tiltakene =
            [ SykkelparkeringUte.tiltakObject
            , LeskurUtenSitteplass.tiltakObject

            {-
               , { name = "Sitteplass på hpl"
                 , page = \model -> [ text "Sitteplass side" ]
                 , toggleVisible = \model -> model
                 }
            -}
            ]
      }
    , { tag = Informasjon
      , tiltakene =
            [ SkiltingIBuss.tiltakObject

            --            , TiltakObject "Hpl. opprop" (\model -> [ text "Hpl. opprop side" ]) (\model -> model)
            , SeparatSykkelveg.tiltakObject
            ]
      }
    ]
