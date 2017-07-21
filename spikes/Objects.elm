module Objects exposing (..)

{-
   type alias FieldType objectType valueType =
       objectType -> (objectType -> valueType)


   sendTo : objectType -> FieldType objectType valueType -> valueType
-}
-- sendTo : Person -> (PersonRecord -> Person -> a) -> a


sendTo ((Person object) as this) field =
    field object this


type Person
    = Person PersonRecord


type alias PersonRecord =
    { fullName : Person -> String
    , firstName : String
    , lastName : String
    , fullTitle : Person -> String -> String
    }


fullNameMethod : Person -> String
fullNameMethod (Person this) =
    this.firstName ++ " " ++ this.lastName


fullTitleMethod : Person -> String -> String
fullTitleMethod this titlePrefix =
    titlePrefix ++ " " ++ (sendTo this .fullName)


makePerson : String -> String -> Person
makePerson firstName lastName =
    Person
        { fullName = fullNameMethod
        , firstName = firstName
        , lastName = lastName
        , fullTitle = fullTitleMethod
        }
