module Main exposing (Person, findTeens, findTeensPrint, main, multiSix, multiply, teen1, teen2, teen3, teen4)

import Html exposing (..)



-- 1: Multiply function


multiply : Int -> Int -> Int
multiply x y =
    case ( x, y ) of
        ( _, 1 ) ->
            x

        ( 1, _ ) ->
            y

        ( _, _ ) ->
            x + multiply x (y - 1)



-- 2: 6 * 8 in currying


multiSix =
    multiply 6

multiSixEightCurrying =
    multiSix 8


-- 3: Person record finder


type alias Person =
    { name : String, age : Int }


findTeens : List Person -> List Person
findTeens list =
    List.filter (\x -> x.age >= 16) list


teen1 =
    Person "Angela" 24


teen2 =
    Person "Stefan" 14


teen3 =
    Person "Johannes" 17


teen4 =
    Person "Rebecca" 13


findTeensPrint =
    findTeens [ teen1, teen2, teen3, teen4 ] |> List.map (\x -> x.name ++ " " ++ String.fromInt x.age) |> String.join ", "



-- Printing


main =
    div []
        [ div []
            [ h3 [] [ text "1" ]
            , p [] [ text (String.fromInt (multiply 8 6)) ]
            ]
        , div []
            [ h3 [] [ text "2" ]
            , p [] [ text (String.fromInt (multiSixEightCurrying)) ]
            ]
        , div []
            [ h3 [] [ text "3" ]
            , p [] [ text findTeensPrint ]
            ]
        ]
