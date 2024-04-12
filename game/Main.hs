module Main where

import Data.Data (Typeable)
import Debug.Trace
import Functions
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main =
  playIO
    (InWindow "hsecs-game" (800, 450) (40, 40))
    white -- bg color
    120 -- fps
    gameInitial -- initial state
    gameView -- view func
    gameInput -- input func
    gameStep -- step func

-- the ecs
type GameState = ECS

-- Position for our entities
data Position = Position Int Int
  deriving (Show, Eq, Typeable)

-- Shape for our entities
data Shape = CircleShape | SquareShape
  deriving (Show, Eq, Typeable)

-- Control mode, how the entity is controlled
data ControlMode = Player | AI
  deriving (Show, Eq, Typeable)

-- player
player :: Entity
player = mkEntity >:> (Position 0 0) >:> CircleShape >:> Player

-- processing functions
processAI :: World -> World
processAI = mapW f
  where
    f (Position x y) = Position (x + 1) y

-- set up game
gameInitial :: GameState
gameInitial =
  let world = mkWorld [player]
   in mkECS world [processAI]

-- draw
gameView :: GameState -> IO Picture
gameView ecs = return $ Text "Hello"

-- step in time
gameStep :: Float -> GameState -> IO GameState
gameStep dt ecs = return $ step ecs

-- input
gameInput :: Event -> GameState -> IO GameState
gameInput ev ecs = return ecs

-- \$ case ev of
-- EventKey (SpecialKey KeyUp) Up _ _ -> step s (\(Position x y) -> Position (x + 1) y)
