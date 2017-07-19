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
