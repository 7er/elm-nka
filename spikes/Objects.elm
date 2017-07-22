module Objects exposing (..)

{-  -}
-- sendTo : Person -> (PersonRecord -> Person -> a) -> a


sendTo ((Tiltak object) as this) field =
    field object this


type Tiltak
    = Tiltak TiltakRecord


type alias TiltakRecord =
    { fullName : Tiltak -> String
    , firstName : Tiltak -> String
    , lastName : Tiltak -> String
    , fullTitle : Tiltak -> String -> String
    }


fullNameMethod : Tiltak -> String
fullNameMethod this =
    (sendTo this .firstName) ++ " " ++ (sendTo this .lastName)


fullTitleMethod : Tiltak -> String -> String
fullTitleMethod this titlePrefix =
    titlePrefix ++ " " ++ (sendTo this .fullName)


firstNameMethodForSomethingElse : Tiltak -> String
firstNameMethodForSomethingElse this =
    "fleskens"


makeTiltak : String -> String -> Tiltak
makeTiltak firstName lastName =
    Tiltak
        { fullName = fullNameMethod
        , firstName = \(Tiltak object) -> firstName
        , lastName = \(Tiltak object) -> lastName
        , fullTitle = fullTitleMethod
        }


makeSomethingElse : String -> Tiltak
makeSomethingElse description =
    Tiltak
        { fullName = fullNameMethod
        , firstName = firstNameMethodForSomethingElse
        , lastName = \(Tiltak object) -> description
        , fullTitle = fullTitleMethod
        }


type Tagged tag value
    = Tagged value


type PercentTag
    = UnusedPercent


type MinutesTag
    = UnusedMinutes


type alias Percent =
    Tagged PercentTag Float


type alias Minutes =
    Tagged MinutesTag Float


percent : Float -> Percent
percent =
    Tagged


minutes : Float -> Minutes
minutes =
    Tagged


percentOfMinute : Percent -> Minutes
percentOfMinute percent =
    percent |> map (\value -> 60 * value) |> retag


untag : Tagged a b -> b
untag (Tagged value) =
    value


retag : Tagged a b -> Tagged c b
retag tagged =
    tagged |> untag |> Tagged


map : (b -> c) -> Tagged a b -> Tagged a c
map func tagged =
    tagged |> untag |> func |> Tagged
