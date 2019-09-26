import Mouse
import Graphics.Element exposing (show)

main = Signal.map show (count Mouse.clicks)
count signal = Signal.foldp (\_ n -> n + 1) 0 signal
