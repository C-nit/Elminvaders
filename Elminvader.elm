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


----------------------------------------------Movements

--moves form to x, y
moveToXY : Int -> Int -> Form -> Form
moveToXY x y =
    move (toFloat x, toFloat y) --(10.0* toFloat x, 10.0* toFloat y)

----------------------------------------------Forms

shot : Form
shot =
    filled Color.red (circle 5.0)

shot2 : Form
shot2 =
    filled Color.orange (circle 5.0)

invaderForm : Form
invaderForm =
    toForm (image 40 30 "images/Ship.png")

----------------------------------------------Gamestate

type Livelyness = Dead | Alive

type alias Dimensions = (Int, Int)

type alias ExistantAt a =
    { a
    | x : Int
    , y : Int
    , livelyness : Livelyness
    }

createExistantAt : Int -> Int -> Livelyness -> ExistantAt {}
createExistantAt a b c=
    { x = a
    , y = b
    , livelyness = c
    }

type alias Defender = ExistantAt {}

createDefender : Int -> Int -> Livelyness -> Defender
createDefender x y live =
    createExistantAt x y live

type alias Invader = ExistantAt {}

createInvader : Int -> Int -> Livelyness -> Invader
createInvader x y live =
    createExistantAt x y live

type alias DefenderFire = ExistantAt {}

createDefenderFire : Int -> Int -> Livelyness -> DefenderFire
createDefenderFire x y live =
    createExistantAt x y live

type alias InvaderFire = ExistantAt {}

createInvaderFire : Int -> Int -> Livelyness -> InvaderFire
createInvaderFire x y live =
    createExistantAt x y live

type Shift = Left | Right

type alias GameState =
    { defender : Defender
    , invaders : List Invader
    , defenderFires: List DefenderFire
    , invaderFires : List InvaderFire
    , dim : Dimensions
    , shift : Shift
    }

--Default Gamestate
gameStart : GameState
gameStart =
    { defender = createDefender 0 -135 Alive   --(Window.height // 2 + 15)
    , invaders = [ createInvader -20 60 Alive, createInvader 30 20 Dead]
    , defenderFires = [ createDefenderFire 120 0 Dead]
    , invaderFires = [ createInvaderFire 120 40 Dead, createInvaderFire 220 40 Alive]
    , dim = (400, 300)
    , shift = Right
    }

--current gameState
gameState : Signal GameState
gameState =
    Signal.foldp updateGame gameStart mergedSignals


----------------------------------------------Signals

--corrects 0,0 and y direction of coordinate system of collage vs mouseposition
corrZero : (Int, Int) -> (Int, Int) -> (Int, Int)
corrZero (x, y) (w, h) =
    (x - w // 2 , -y + h //2 )

--All Signals !! order is of importance, left one has priority
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

----------------------------------------------Update

-- could be updated so state.dim would be considered
inRange : ExistantAt a -> Livelyness
inRange thing =
    if thing.livelyness == Dead
        then Dead
    else if (thing.y < 540 && thing.y > -540)
        then Alive
    else Dead

isAlive : ExistantAt a -> Bool
isAlive thing =
    if thing.livelyness == Alive
        then True
    else False

bury : List (ExistantAt a) -> List (ExistantAt a)
bury  list =
    List.filter (isAlive) list

updateDefFire : DefenderFire -> DefenderFire
updateDefFire fire =
    createDefenderFire fire.x (fire.y + 2) (inRange fire)

updateInvFire : InvaderFire -> InvaderFire
updateInvFire fire =
    createInvaderFire fire.x (fire.y - 1) (inRange fire)

--not ready dir?
updateInvader : Shift -> Int -> Invader -> Invader
updateInvader dir speed inv =
    if dir == Right
        then createInvader (inv.x + speed) inv.y (inRange inv)
    else createInvader (inv.x - speed) inv.y (inRange inv)

newDefFire : List DefenderFire -> Defender -> List DefenderFire
newDefFire fires player =
    createDefenderFire player.x player.y Alive :: fires

updateInvaderTick : Invader -> Invader
updateInvaderTick inv =
    createInvader inv.x (inv.y - 5) inv.livelyness

updateShift : Shift -> Shift
updateShift dir =
    if dir == Right
        then Left
    else Right

-- UNFINISHED!!
updateGame : Update -> GameState -> GameState
updateGame update state =
    case update of
        MouseMove (a, _) ->
            { state
            | defender <- createDefender a (-(snd state.dim) // 2 + 15) state.defender.livelyness
            }
        DimDelta wh ->
            { state
            | dim <- wh
            }
        TimeDelta _ ->
            { state
            | defenderFires <- bury (List.map updateDefFire state.defenderFires)
            , invaderFires  <- bury (List.map updateInvFire state.invaderFires)
            , invaders      <- bury (List.map( updateInvader state.shift 1) state.invaders)
            }
        Click ->
            { state
            | defenderFires <- newDefFire state.defenderFires state.defender
            }
        Tick ->
            { state
            | invaders <- (List.map updateInvaderTick state.invaders)
            , shift <- updateShift state.shift
            }

---------------------------------------------- View functions

viewDefender : Defender -> Form
viewDefender defender =
    moveToXY defender.x defender.y invaderForm --Debug.watch "Defendery"

viewInvader : Invader -> Form
viewInvader invader =
    moveToXY invader.x invader.y invaderForm

viewInvaders : List Invader  -> List Form
viewInvaders invaders =
    List.map viewInvader invaders

viewDefenderFire : DefenderFire -> Form
viewDefenderFire defenderFire =
    moveToXY defenderFire.x defenderFire.y shot

viewDefenderFires : List DefenderFire -> List Form
viewDefenderFires defenderFires =
    List.map viewDefenderFire defenderFires

viewInvaderFire : InvaderFire -> Form
viewInvaderFire invaderFire =
    moveToXY invaderFire.x invaderFire.y shot2

viewInvaderFires : List InvaderFire -> List Form
viewInvaderFires invaderFires =
    List.map viewInvaderFire invaderFires

viewGame : GameState -> Element
viewGame state =
    collage
        (fst state.dim)
        (snd state.dim)
        (viewDefender state.defender
            :: viewInvaders state.invaders
            |> List.append (viewDefenderFires state.defenderFires)
            |> List.append (viewInvaderFires state.invaderFires)
            )

---------------------------------------------- Main

main : Signal Element
main = Signal.map viewGame gameState
