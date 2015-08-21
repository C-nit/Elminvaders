module Main where
import Mouse
--import Keyboard
import Signal
import Time exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color
import Window

elminvdir : String
elminvdir = "C:/Users/Zenit/Desktop/ElmInvaders"

--corrects 0,0 of coordinate system of collage vs mouseposition
corrzero : Int -> Int -> Int
corrzero a size =  a - size //2

----------------------------------------------Movements
--moves form to x, y
movetoxy : Int -> Int -> Form -> Form
movetoxy x y = move (toFloat x, toFloat y) --(10.0* toFloat x, 10.0* toFloat y)

movetox : Int -> Form -> Form
movetox x = moveX (toFloat x)

movetoy : Int -> Form -> Form
movetoy y = moveY (toFloat y)

--Element to From (forms can be moved, elements not)
--toForm : Element -> Form
--image : Int -> Int -> String -> Element
--floor --roun
----------------------------------------------Forms

chugle : Form
chugle = filled Color.red (circle 5.0)

--Form moving to x y   --mousepos.y is inverted
chuglexy : ( Int, Int) -> ( Int, Int)  -> Form
chuglexy (w, h) (x, y) = movetoxy (corrzero x w) -(corrzero y h) chugle

invader : Form
invader = toForm (image 30 30 ( elminvdir ++ "/spaceinvaders.jpg" ))

--invader moving to x at y
invaderx : Int -> ( Int, Int) -> Form
invaderx x (w, h) = movetoxy (corrzero x w) (-h//2 + 15) invader -- -

timestep : Float -> Int
timestep = floor inSeconds(every 1 )

invaderstep : Form
invaderstep  = movetoy (15*timestep) invader

formlist : ( Int, Int) -> ( Int, Int)  -> List Form
formlist (w, h) (x, y) = [ chuglexy (w, h)  (x, y), invaderx x (w, h), invaderstep]

view : (Int, Int) -> ( Int, Int) ->  Element   -- { x : Int, y : Int}
--view (x, y) = text ((toString x) ++ ", "  ++ (toString y))   --mauspo anzeigen
view (w, h) (x, y) = collage w h (formlist (w, h) (x, y) )


main : Signal Element
main = Signal.map2 view Window.dimensions Mouse.position --Keyboard.arrows
