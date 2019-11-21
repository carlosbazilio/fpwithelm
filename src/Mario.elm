-- Walk around with the arrow keys. Press the UP arrow to jump!
--
-- Learn more about the playground here:
--   https://package.elm-lang.org/packages/evancz/elm-playground/latest/
--


import Playground exposing (..)
import Html exposing (..)
import Tuple exposing (..)

import Debug exposing (toString)


-- MAIN

numCoins = 4

main =
  game view update
    { x = -100
    , y = 0
    , vx = 0
    , vy = 0
    , dir = "right"
    , voldx = 0
    , score = 0
    , coins = List.map (\x -> (x * 80, floor)) (range 1 numCoins)
    , barriers = List.map (\x -> ((x * 80) + 40, floor)) (range 1 (numCoins-1))
    , gameover = False
    }

range : number -> number -> List number
range x y =
  if x <= y then x :: range (x + 1) y
  else [] 


-- VIEW

floor = 30 --76
tamMario = 70

--coinGifUrl = "https://ukikipedia.net/mediawiki/images/5/5d/Yellow_Coin.gif"
coinGifUrl = "../img/Yellow_Coin.gif"
audioMarioThemeUrl = "https://archive.org/download/SuperMarioBros.ThemeMusic/SuperMarioBros.mp3"

drawcoin bottom (x, y) =
  image 20 20 coinGifUrl |> move x (bottom + floor)

drawCoins mario bottom =
  List.map (drawcoin bottom) mario.coins


placar mario w h =
  group
    [ rectangle grey 60 60
    , words red (String.fromInt mario.score) |> scale 3
    ] |> move (w / 2.5) (h / 2.5)

block = (10,60)

drawBlock bottom (x,y) =
  rectangle brown (first block) (second block) |> move x (bottom + floor)

drawBlocks mario bottom =
  List.map (drawBlock bottom) mario.barriers

gameIsOver w h =
  image w h "../img/macaetech.png"  


view computer mario =
  let
    w = computer.screen.width
    h = computer.screen.height
    b = computer.screen.bottom
  in
  if (not (mario.gameover)) then
    [ rectangle (rgb 174 238 238) w h
    , rectangle (rgb 74 163 41) w 100
        |> moveY (b - 45)
    , image tamMario tamMario (toGif mario)
        |> move mario.x (b + floor + mario.y)
    , placar mario w h
    -- Debug
    --, words black ((toString mario.x) ++ "//" ++ (toString mario.y) ++ "//"
    --++ String.fromFloat (b + floor + mario.y) ++ "//" 
    --++ (toString (List.head mario.coins)) ++ "//"
    --++ (toString mario.vx))
    ] ++ (drawBlocks mario b) ++ drawCoins mario b
  else
    [ gameIsOver w h ]


--marioGifsUrl = "https://elm-lang.org/images/mario/"
marioGifsUrl = "../img/"

toGif mario =
  marioGifsUrl ++ (
    if mario.y > 0 then
      "jump/"
    else if mario.vx /= 0 then
      "walk/"
    else
      "stand/" 
  ) ++ mario.dir ++ ".gif"


-- UPDATE

interceptMario mario (x, y) = 
  (abs (mario.x - x)) < (tamMario / 2) &&
    (abs (mario.y - y)) < (tamMario / 2)

notInterceptMario mario (x, y) = 
  not (interceptMario mario (x, y))

remainingCoins mario =
  List.filter (notInterceptMario mario) mario.coins

touchBarrier mario =
  List.filter (interceptMario mario) mario.barriers |> \x -> (List.length x) > 0

update computer mario =
  let
    dt = 1.666
    vx = toX computer.keyboard
    vy =
      if mario.y == 0 then
        if computer.keyboard.up then 5 else 0
      else
        mario.vy - dt / 12
    --x = mario.x + dt * vx
    voldx =
      if (touchBarrier mario) then
        if (mario.voldx == 0) then
          mario.vx
        else
          mario.voldx
      else
        0
    x =
      if (touchBarrier mario) && (mario.y == 0) then
        if ((mario.voldx > 0) && (computer.keyboard.left) ||
            (mario.voldx < 0) && (computer.keyboard.right)) then
          mario.x + dt * vx
        else
          mario.x
      else
        mario.x + dt * vx
    --y = mario.y + dt * vy
    y =
      if (touchBarrier mario) && (mario.y > (second block)) then
        if (computer.keyboard.up) then
          mario.y + dt * vy
        else
          mario.y
      else
        mario.y + dt * vy
    coins = remainingCoins mario
    score = numCoins - (List.length coins)
    barriers = mario.barriers
    gameover = mario.score == numCoins
  in
  { x = x
  , y = max 0 y
  , vx = vx
  , vy = vy
  , dir = if vx == 0 then mario.dir else if vx < 0 then "left" else "right"
  , voldx = voldx
  , score = score
  , coins = coins
  , barriers = barriers
  , gameover = gameover
  }