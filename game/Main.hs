module Main where

import Data.Data (Typeable)
import Data.Maybe
import Debug.Trace
import Functions
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

main :: IO ()
main = do
  initialState <- gameInitial
  playIO
    (InWindow "hsecs-game" (800, 450) (40, 40))
    white -- bg color
    60 -- fps
    initialState -- initial state
    gameView -- view func
    gameInput -- input func
    gameStep -- step func

-- enemy count
enemyCount :: Int
enemyCount = 40

-- the ecs
type GameState = ECS

-- Position for our entities
data Position = Position Int Int
  deriving (Show, Eq, Typeable)

-- This is a player
data Player = Player
  deriving (Show, Eq, Typeable)

-- Enemy, and where it respawns
data Enemy = Enemy Int Int
  deriving (Show, Eq, Typeable)

-- Dying animation
data DeathAnim = Dying Int
  deriving (Show, Eq, Typeable)

-- player
player :: Entity
player = mkEntity >:> (Position 0 0) >:> Player

-- enemy entity
enemy :: IO Entity
enemy = do
  spawnX <- randomIO
  spawnY <- randomIO
  let spawnX' = (spawnX `mod` 1600) - 800
  let spawnY' = (spawnY `mod` 900) - 455
  return $ mkEntity >:> (Position spawnX' spawnY') >:> Enemy spawnX' spawnY'

-- make the AI follow the player
followPlayer :: World -> World
followPlayer = doubleQW f
  where
    f (Position x y, Player) (Position x' y', Enemy _ _) = Position (dir x x') (dir y y')
    dir a b
      | a < b = b - 1
      | a > b = b + 1
      | otherwise = b

-- Dying animation
dyingAnimation :: World -> World
dyingAnimation = mapW f
  where
    f (Position x y, Enemy x' y', Dying t)
      | t <= 0 = (Position x' y', Dying 0)
      | otherwise = (Position x y, Dying $ t - 1)

-- set up game
gameInitial :: IO GameState
gameInitial = do
  enemies <- sequence $ take enemyCount (repeat enemy)
  let world = mkWorld (player : enemies)
  return $ mkECS world [followPlayer]

-- draw
gameView :: GameState -> IO Picture
gameView ecs =
  let players = collectW drawPlayer ecs
      enemies = collectW drawEnemy ecs
      dying = collectW drawDying ecs
   in return $ Pictures (players ++ enemies ++ dying)
  where
    tof x = fromIntegral x :: Float
    drawPlayer (Position x y, Player) = Color black $ Translate (tof x) (tof y) $ Circle 8.0
    drawEnemy (Position x y, Enemy _ _) = Color red $ Translate (tof x) (tof y) $ Circle 10.0
    drawDying (Position x y, Dying t) = Color red $ Translate (tof x) (tof y) $ Pictures [Line [(15.0 - tof t, -15.0 + tof t), (-15.0 + tof t, 15.0 - tof t)]]

-- step in time
gameStep :: Float -> GameState -> IO GameState
gameStep dt ecs = return $ step ecs

-- input
gameInput :: Event -> GameState -> IO GameState
gameInput ev ecs = return ecs

-- \$ case ev of
-- EventKey (SpecialKey KeyUp) Up _ _ -> step s (\(Position x y) -> Position (x + 1) y)
