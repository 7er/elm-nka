module Objects exposing (..)

{-
   type alias FieldType objectType valueType =
       objectType -> (objectType -> valueType)


   sendTo : objectType -> FieldType objectType valueType -> valueType
-}
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
