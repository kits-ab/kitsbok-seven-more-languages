module Main exposing (..)
import Html exposing (..)

-- Allow "null" age, using maybe monad

type alias Person =
    { name : String, age : Maybe Int }

peter = Person "Peter" (Just 17)
ida = Person "Ida" (Just 1)
sophie = Person "Sophie" Nothing

findTeens : List Person -> List Person
findTeens list =
    List.filter findTeensFilter list

findTeensFilter person =
    case person.age of
        Nothing -> False
        Just x -> x > 16

findTeensPrint =
    findTeens [ peter, ida, sophie ] |> List.map (\x -> x.name) |> String.join ", "

main = text findTeensPrint