import Graphics.Element exposing (show) 
import Mouse

clickPosition = Signal.sampleOn Mouse.clicks Mouse.position 
main = Signal.map show clickPosition