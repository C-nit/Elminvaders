module Main where
import Mouse
--import Keyboard
import Signal
import Time exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color
import Window

--corrects 0,0 of coordinate system of collage vs mouseposition
corrzero : Int -> Int -> Int
corrzero a size =
    a - size // 2
----------------------------------------------Gamestate
{--}
type Livelyness = Dead | Alive

type alias ExistantAt =
    { x : Int
    , y : Int
    , livelyness : Livelyness
    }

type alias Defender = --ExistantAt
    { x : Int
    , y : Int
    , livelyness : Livelyness
    }

type alias Invader = --ExistantAt
    { x : Int
    , y : Int
    , livelyness : Livelyness
    }

type alias DefenderFire = --ExistantAt
    { x : Int
    , y : Int
    , livelyness : Livelyness
    }

type alias GameState =
    { defender : Defender
    , invader : List Invader
--    , defenderFire: DefenderFire
--    , invaderFire : List InvaderFire
    }

--}

--Default Gamestate
defGameState : GameState
defGameState =
    { defender = Defender 0 0 Alive
    , invader = [ Invader 0 0 Alive]
    }

----------------------------------------------Movements
--moves form to x, y
moveToXY : Int -> Int -> Form -> Form
moveToXY x y =
    move (toFloat x, toFloat y) --(10.0* toFloat x, 10.0* toFloat y)

moveToX : Int -> Form -> Form
moveToX x =
    moveX (toFloat x)

movetoY : Int -> Form -> Form
movetoY y =
    moveY (toFloat y)

--Element to From (forms can be moved, elements not)
--toForm : Element -> Form
--image : Int -> Int -> String -> Element
--floor --roun
----------------------------------------------Forms

chugle : Form
chugle =
    filled Color.red (circle 5.0)

--Form moving to x y   --mousepos.y is inverted
chugleXY : (Int, Int) -> (Int, Int) -> Form
chugleXY (w, h) (x, y) =
    moveToXY (corrzero x w) -(corrzero y h) chugle

invader : Form
invader =
    toForm (image 40 30 "images/Ship.png")

--invader moving to x at y
invaderX : Int -> (Int, Int) -> Form
invaderX x (w, h) =
    moveToXY (corrzero x w) (-h // 2 + 15) invader

invaderStep : Time -> Form
invaderStep t =
    movetoY -10 invader


formlist : (Int, Int) -> (Int, Int) -> Time -> List Form
formlist (w, h) (x, y) t =
    [ chugleXY (w, h)  (x, y)
    , invaderX x (w, h)
    , invaderStep t
    ]

view : (Int, Int) -> (Int, Int) -> Time -> Element   -- { x : Int, y : Int}
--view (x, y) = text ((toString x) ++ ", "  ++ (toString y))   --mauspo anzeigen
view (w, h) (x, y) t =
    collage w h
    <| formlist (w, h) (x, y) t


main : Signal Element
main =
    Signal.map3 view Window.dimensions Mouse.position (fps 30 ) --Keyboard.arrows
