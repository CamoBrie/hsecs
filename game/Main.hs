module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO 
  (InWindow "hsecs-game" (800, 450) (40, 40))
  white -- bg color
  120 -- fps
  initial -- initial state
  view -- view func
  input -- input func
  step -- step func

type GameState = ()

-- set up game
initial :: GameState
initial = ()

-- draw
view :: GameState -> IO Picture
view s = return $ Text "Hello"

-- step in time
step :: Float -> GameState -> IO GameState
step dt s = return s

-- input
input :: Event -> GameState -> IO GameState
input ev s = return s
  
