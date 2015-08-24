module Main where
import Mouse
--import Keyboard
import Signal
import Time exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color
import Window
import Debug

--corrects 0,0 of coordinate system of collage vs mouseposition
corrZero : (Int, Int) -> (Int, Int) -> (Int, Int)
corrZero (x, y) (w, h) =
    (x - w // 2 , -y + h //2 )
----------------------------------------------Gamestate
{--}
type Livelyness = Dead | Alive

type alias Dimensions = (Int, Int)

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
    , invaders : List Invader
--    , defenderFire: DefenderFire
--    , invaderFire : List InvaderFire
    , dim : Dimensions
    }

--}

--Default Gamestate
gameStart : GameState
gameStart =
    { defender = Defender 0 -135 Alive   --(Window.height // 2 + 15)
    , invaders = [ Invader -20 0 Alive]
    , dim = (400, 300)
    }

--All Signals !! order is of importance, left one
type Update =   DimDelta (Int, Int)
                | MouseMove (Int, Int)
                | TimeDelta Time
                | Click
                | Tick
mergedSignals : Signal Update
mergedSignals =
    Signal.mergeMany
        [ Signal.map DimDelta Window.dimensions
        , Signal.map2 corrZero Mouse.position Window.dimensions |> Signal.map MouseMove
        , Signal.map TimeDelta (fps 30)
        , Signal.map (always Click) Mouse.clicks
        , Signal.map (always Tick) (Time.every Time.second)
        ]

--current gameState
gameState : Signal GameState
gameState =
    Signal.foldp updateGame gameStart mergedSignals

-- UNFINISHED!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
updateGame : Update -> GameState -> GameState
updateGame update state =
    case update of
        MouseMove (a, _) ->
            { state
            | defender <- Defender a (-(snd state.dim) // 2 + 15) state.defender.livelyness
            }
        DimDelta wh ->
            { state
            | dim <- wh
            }
        TimeDelta _ -> state
        Click -> state
        Tick -> state




viewDefender : Defender -> Form
viewDefender defender =
    defenderXY (Debug.watch "Defender" defender.x) (Debug.watch "Defendery" defender.y)

viewInvader : Invader -> Form
viewInvader invader =
    invaderXY invader.x invader.y

viewInvaders : List Invader  -> List Form
viewInvaders invaders =
    List.map viewInvader invaders

viewGame : GameState -> Element
viewGame state =
    collage
        (fst state.dim)
        (snd state.dim)
        (viewDefender state.defender
            :: viewInvaders state.invaders)






main : Signal Element
main = Signal.map viewGame gameState

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


invader : Form
invader =
    toForm (image 40 30 "images/Ship.png")

--invader moving to x at y
defenderXY : Int -> Int -> Form
defenderXY x y=
    moveToXY x y invader

invaderXY : Int -> Int -> Form
invaderXY x y =
    moveToXY x y invader

{-}
chugle : Form
chugle =
    filled Color.red (circle 5.0)

--Form moving to x y   --mousepos.y is inverted
chugleXY : (Int, Int) -> (Int, Int) -> Form
chugleXY (w, h) (x, y) =
    moveToXY (corrZero x w) -(corrZero y h) chugle

invader : Form
invader =
    toForm (image 40 30 "images/Ship.png")

--invader moving to x at y
defenderXY : Int -> (Int, Int) -> Form
defenderXY x (w, h) =
    moveToXY (corrZero x w) (-h // 2 + 15) invader

invaderXY : Int -> Int -> (Int, Int) -> Form
invaderXY x y (w, h) =
    moveToXY (corrZero x w) (corrZero y h) invader

invaderStep : Time -> Form
invaderStep t =
    movetoY -10 invader


formlist : (Int, Int) -> (Int, Int) -> Time -> List Form
formlist (w, h) (x, y) t =
    [ chugleXY (w, h)  (x, y)
    , defenderXY x (w, h)
    , invaderStep t
    ]

view : (Int, Int) -> (Int, Int) -> Time -> Element   -- { x : Int, y : Int}
--view (x, y) = text ((toString x) ++ ", "  ++ (toString y))   --mauspo anzeigen
view (w, h) (x, y) t =
    collage w h
    <| formlist (w, h) (x, y) t
--}

--main : Signal Element
--main =
--    Signal.map3 view Window.dimensions Mouse.position (fps 30 ) --Keyboard.arrows
