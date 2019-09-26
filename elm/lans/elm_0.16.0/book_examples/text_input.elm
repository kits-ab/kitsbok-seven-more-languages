import String
import Html exposing (Html, Attribute, text, toElement, div, input) 
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Signal exposing (Address)
import StartApp.Simple as StartApp

main = StartApp.start { model = "", view = view, update = update }

update newStr oldStr = newStr
shout text = String.toUpper text
whisper text = String.toLower text
echo text = (shout text) ++ " " ++ (whisper text)

view address string = div []
    [ input
        [ placeholder "Speak"
        , value string
        , on "input" targetValue (Signal.message address) , myStyle
        ]
        []
    , div [ myStyle ] [ text (echo string) ]
    ]
    
myStyle = style [ ("width", "100%") ]