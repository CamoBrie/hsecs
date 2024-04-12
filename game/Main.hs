{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Data.List
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

-- player speed
playerSpeed :: Int
playerSpeed = 5

-- the ecs
type GameState = ECS

-- Position for our entities
data Position = Position Int Int
  deriving (Show, IsComponent)

-- This is a player
data Player = Player
  deriving (Show, IsComponent)

-- Enemy, and where it respawns
data Enemy = Enemy Int Int
  deriving (Show, IsComponent)

-- Dying animation
data DeathAnim = Dying Int
  deriving (Show, IsComponent)

-- Attack animation
data Attack = Attack Int
  deriving (Show, IsComponent)

-- Move direction for the player
data MoveDir
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  | DoAttack
  deriving (Show, Eq, IsComponent)

-- Stack of move directions
data MoveStack = MoveStack [MoveDir]
  deriving (Show, IsComponent)

-- Score
data Score = Score Int
  deriving (Show, IsComponent)

-- player
player :: Entity
player = mkEntity >:> (Position 0 0) >:> Player >:> (MoveStack [])

-- enemy entity
enemy :: IO Entity
enemy = do
  spawnX <- randomIO
  spawnY <- randomIO
  let spawnX' = (spawnX `mod` 1600) - 800
  let spawnY' = (spawnY `mod` 900) - 455
  return $ mkEntity >:> (Position spawnX' spawnY') >:> Enemy spawnX' spawnY'

-- Score
scoreboard :: Entity
scoreboard = mkEntity >:> Score 0

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

-- Handle player movement
movePlayer :: World -> World
movePlayer = mapW f
  where
    f (MoveStack (MoveUp : xs), Position x y, Player) = Position x (y + playerSpeed)
    f (MoveStack (MoveDown : xs), Position x y, Player) = Position x (y - playerSpeed)
    f (MoveStack (MoveLeft : xs), Position x y, Player) = Position (x - playerSpeed) y
    f (MoveStack (MoveRight : xs), Position x y, Player) = Position (x + playerSpeed) y
    f (_, Position x y, Player) = Position x y

-- Add the attack
attack :: World -> World
attack = mapW f
  where
    f (MoveStack (DoAttack : xs), Player) = (MoveStack xs, Attack 10)
    f (xs, p) = (xs, Attack 0) -- Bit of cruft, this should get removed directly after

-- Remove the attack when done
removeAttack :: World -> World
removeAttack = mapW f
  where
    f :: Attack -> Maybe (Remove Attack)
    f (Attack n) = if n <= 0 then Just Remove else Nothing

-- Attack countdown
countAttack :: World -> World
countAttack = mapW f
  where
    f (Attack n) = Attack $ n - 1

-- set up game
gameInitial :: IO GameState
gameInitial = do
  enemies <- sequence $ take enemyCount (repeat enemy)
  let world = mkWorld (player : scoreboard : enemies)
  return $ mkECS world [followPlayer, dyingAnimation, movePlayer, attack, removeAttack, removeAttack]

-- draw
gameView :: GameState -> IO Picture
gameView ecs =
  let players = collectW drawPlayer ecs
      enemies = collectW drawEnemy ecs
      dying = collectW drawDying ecs
      score = collectW drawScore ecs
      attack = collectW drawAttack ecs
   in return $ Pictures (players ++ enemies ++ dying ++ score ++ attack)
  where
    tof x = fromIntegral x :: Float
    drawPlayer (Position x y, Player) = Color black $ Translate (tof x) (tof y) $ Circle 8.0
    drawEnemy (Position x y, Enemy _ _) = Color red $ Translate (tof x) (tof y) $ Circle 10.0
    drawDying (Position x y, Dying t) = Color red $ Translate (tof x) (tof y) $ Line [(15.0 - tof t, -15.0 + tof t), (-15.0 + tof t, 15.0 - tof t)]
    drawAttack (Position x y, Attack _) = Color blue $ Line [(-1000.0, tof y), (1000.0, tof y)]
    drawScore (Score t) = Translate (-400.0) (200.0) $ Scale 0.2 0.2 $ Text $ "Score: " ++ show t

-- step in time
gameStep :: Float -> GameState -> IO GameState
gameStep dt ecs = return $ step ecs

-- input
gameInput :: Event -> GameState -> IO GameState
gameInput ev ecs = return $ runSys (mapW f) ecs
  where
    f (MoveStack xs) = case ev of
      -- Move
      EventKey (SpecialKey KeyUp) Down _ _ -> MoveStack $ MoveUp : xs
      EventKey (SpecialKey KeyDown) Down _ _ -> MoveStack $ MoveDown : xs
      EventKey (SpecialKey KeyLeft) Down _ _ -> MoveStack $ MoveLeft : xs
      EventKey (SpecialKey KeyRight) Down _ _ -> MoveStack $ MoveRight : xs
      -- Don't move
      EventKey (SpecialKey KeyUp) Up _ _ -> MoveStack $ delete MoveUp xs
      EventKey (SpecialKey KeyDown) Up _ _ -> MoveStack $ delete MoveDown xs
      EventKey (SpecialKey KeyLeft) Up _ _ -> MoveStack $ delete MoveLeft xs
      EventKey (SpecialKey KeyRight) Up _ _ -> MoveStack $ delete MoveRight xs
      -- Attack
      EventKey (SpecialKey KeySpace) Down _ _ -> MoveStack $ DoAttack : xs
      -- other
      _ -> MoveStack $ xs
