module ClassSystem exposing (..)


foo self x =
    self.y + x


bar self y =
    self.y * y


bind data class =
    { foo = class.foo data
    , bar = class.bar data
    }


demo =
    let
        class =
            { foo = foo, bar = bar }
    in
        bind { y = 3 } class


demoDerived =
    let class =
            { foo = foo, bar
