module Main exposing (Address, Address2(..), Person, Person2(..), Point, berit, easy1print, easy2print, getXs, johan, listProduct, main, p1, p2, p3)

import Html exposing (..)



-- Easy
-- 1: List product


listProduct : List number -> number
listProduct list =
    case list of
        [] ->
            0

        head :: [] ->
            head

        head :: tail ->
            head * listProduct tail


easy1print =
    String.fromInt (listProduct [ 2, 3, 4 ])



-- 2: Get point X


type alias Point =
    { x : Int }


getXs : List Point -> List Int
getXs list =
    case list of
        [] ->
            []

        head :: tail ->
            head.x :: getXs tail


p1 =
    { x = 1 }


p2 =
    { x = 2 }


p3 =
    { x = 3 }


easy2print =
    List.map String.fromInt (getXs [ p1, p2, p3 ]) |> String.join ", "



-- 3: person datatype
-- Record


type alias Address =
    { street : String, zipcode : Int, city : String }


type alias Person =
    { name : String, age : Int, address : Address }


johan =
    { name = "Johan", age = 24, address = { street = "Åvägen 24", zipcode = 41250, city = "Göteborg" } }



-- Custom type


type Address2
    = AddressType String Int String


type Person2
    = PersonType String Int Address2


berit =
    PersonType "Berit" 42 (AddressType "Övägen 42" 41290 "Mölndal")



-- 4: Which is easier? I think record is better for this kind of data.
-- Printing


main =
    div []
        [ div []
            [ h3 [] [ text "Uppgift 1" ]
            , p [] [ text easy1print ]
            ]
        , div []
            [ h3 [] [ text "Uppgift 2" ]
            , p [] [ text easy2print ]
            ]
        ]
