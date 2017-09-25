port module Ports exposing (..)


type alias DomId =
    String


type alias GraphData =
    { domId : DomId
    , data : List ( Float, Float )
    , variableTitle : String
    }


port generateC3 : GraphData -> Cmd msg


port destroyC3 : DomId -> Cmd msg


port updateC3 : GraphData -> Cmd msg
