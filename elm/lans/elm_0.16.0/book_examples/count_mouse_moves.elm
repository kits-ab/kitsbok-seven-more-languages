import Mouse
import Graphics.Element exposing (show)

count signal = Signal.foldp (\_ n -> n + 1) 0 signal 
main = Signal.map show (count Mouse.position)
