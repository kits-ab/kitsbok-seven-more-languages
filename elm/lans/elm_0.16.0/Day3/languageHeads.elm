module LanguageHead where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (Element, image, leftAligned) 
import Keyboard
import List exposing ((::), all, filter, length)
import Mouse
import Random exposing (Seed, generate, initialSeed, int)
import Signal exposing (Signal, foldp, map, sampleOn, map3) 
import Text exposing (color, fromString, height, monospace) 
import Time exposing (Time, every, fps, inSeconds)

type State = Play | Pause | GameOver

type alias Input = { space:Bool, x:Int, delta:Time }
type alias Head = { x:Float, y:Float, vx:Float, vy:Float, img:String }
type alias Player = { x:Float, score:Int }
type alias Game = { state:State, heads: List Head, player:Player, seed: Seed }

defaultHead n = {x=100.0, y=75, vx=60, vy=0.0, img=headImage n }

defaultGame = { state = Pause, 
                heads = [],
                player  = {x=0.0, score=0},
                seed    = initialSeed 1234 }

headImage n = 
    case n of
        0 -> "/Day3/img/brucetate.png"
        1 -> "/Day3/img/davethomas.png"
        2 -> "/Day3/img/evanczaplicki.png"
        3 -> "/Day3/img/joearmstrong.png"
        4 ->  "/Day3/img/josevalim.png"
        otherwise -> ""

bottom = 550

secsPerFrame = 1.0 / 50.0

delta = map inSeconds (fps 50)

input = sampleOn delta (map3 Input Keyboard.space Mouse.x delta)

main = map display gameState

gameState = foldp stepGame defaultGame input

stepGame input game = 
    case game.state of
        Play -> stepGamePlay input game
        Pause -> stepGamePaused input game 
        GameOver -> stepGameFinished input game

stepGamePlay {space, x, delta} ({state, heads, player, seed} as game) = 
    let (rand, seed') =
        generate (int 0 4) seed
    in 
        { game  | state =  stepGameOver x heads
                , heads =  stepHeads heads delta x player.score rand
                , player = stepPlayer player x heads
                , seed  =  seed' }

stepGameOver x heads =
    if allHeadsSafe (toFloat x) heads then Play else GameOver
allHeadsSafe x heads =
    all (headSafe x) heads
headSafe x head =
    head.y < bottom || abs (head.x - x) < 50

stepHeads heads delta x score rand = 
    spawnHead score heads rand
    |>  bounceHeads
    |>  removeComplete
    |>  moveHeads delta

spawnHead score heads rand =
    let addHead = 
        length heads < (score // 5000 + 1) && all (\head -> head.x > 107.0) heads 
    in 
        if addHead then defaultHead rand :: heads else heads

bounceHeads heads = List.map bounce heads
bounce head = 
    { head | vy =  if head.y > bottom && head.vy > 0
                    then -head.vy * 0.95 
                    else head.vy }

removeComplete heads = filter (\x -> not (complete x)) heads

complete {x} = x > 750

moveHeads delta heads = List.map moveHead heads

moveHead ({x, y, vx, vy} as head) =
      { head | x = x + vx * secsPerFrame
             , y = y + vy * secsPerFrame
             , vy = vy + secsPerFrame * 400 }


stepPlayer player mouseX heads =
    { player | score = stepScore player heads
               , x = toFloat mouseX }

stepScore player heads = 
    player.score + 
    1 +
    1000 * (length (filter complete heads))

stepGamePaused {space, x, delta} ({state, heads, player, seed} as game) = 
    { game  | state = stepState space state
            , player = { player| x=toFloat x } }

stepGameFinished {space, x, delta} ({state, heads, player, seed} as game) = 
    if space 
    then defaultGame 
    else { game | state = GameOver, player = { player| x=toFloat x } }

stepState space state = if space then Play else state

display ({state, heads, player, seed} as game) = 
    let (w, h) = (800, 600)
    in collage w h
           ([ drawRoad w h
           , drawBuilding w h
           , drawPaddle w h player.x
           , drawScore w h player
           , drawMessage w h state] ++
           (drawHeads w h heads))
drawRoad w h =
    filled gray (rect (toFloat w) 100) |> moveY (-(half h) + 50)

drawBuilding w h =
      filled red (rect 100 (toFloat h))
      |> moveX (-(half w) + 50)

drawHeads w h heads = List.map (drawHead w h) heads

drawHead w h head =
    let x = half w - head.x
        y = half h - head.y
        src = head.img
    in  toForm (image 75 75 src)
        |> move (-x, y)
        |> rotate (degrees (x * 2 - 100))

drawPaddle w h x =
    filled black (rect 80 10) 
    |> moveX (x + 10 - half w) 
    |> moveY (-(half h - 30))

half x = toFloat x / 2

drawScore w h player = 
    toForm (fullScore player)
    |> move (half w - 150, half h - 40)

fullScore player = txt (height 50) (toString player.score)
txt f = leftAligned << f << monospace << color blue << fromString

drawMessage w h state =
    toForm (txt (height 50) (stateMessage state)) 
    |> move (50, 50)

stateMessage state =
    if state == GameOver then "Game Over" else "Language Head"