module FuncProg exposing (..)


true x y =
    x


false x y =
    y


ifThenElse boolFunc t e =
    boolFunc t e


demo =
    ifThenElse true
        "Det er sant"
        "Det er falskt"


two f x =
    f (f x)


one f x =
    f x


zero f x =
    x


type alias Rec =
    { bar : Float
    , foo : String
    }



--getValue : Rec -> String ->


getStringValue : Rec -> String -> (a -> String) -> Result String String
getStringValue rec string func =
    case string of
        "bar" ->
            Ok (func rec.bar)

        "foo" ->
            Ok (func rec.foo)

        _ ->
            Err (string ++ " not found")
