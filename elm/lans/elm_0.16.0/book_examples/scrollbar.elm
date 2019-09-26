import Graphics.Element exposing (show) 
import Mouse
import Window

div x y = show ((toFloat x) / (toFloat y)) 
main = Signal.map2 (div) Mouse.y Window.height