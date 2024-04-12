{-# LANGUAGE DeriveAnyClass #-}

module Example where

import Data.Data (Typeable)
import Functions
  ( Entity,
    World,
    doubleQW,
    mapW,
    mkECS,
    mkEntity,
    mkWorld,
    printState,
    step,
    (>:>),
  )
import SystemResults (SpawnEntity, Remove (Remove))
import Types (IsComponent)

---- COMPONENTS ----

data Player = Player deriving (Show, Eq, Typeable, IsComponent)

-- | Position component
data Position = Position
  { pos_x :: Int,
    pos_y :: Int
  }
  deriving (Show, IsComponent)

-- | Name component
data Name
  = Name
      { name :: String
      }
  | Name2
      { name :: String
      }
  deriving (Show, IsComponent)

---- CONSTRUCTION ----

e1 :: Entity
e1 = mkEntity >:> (Position 0 0) >:> (Name "Entity") >:> Player

e2 :: Entity
e2 = mkEntity >:> (Position 1 1) >:> (Name2 "Entity2")

e3 :: Entity
e3 = mkEntity >:> (Position 2 2) >:> (Name "Entity3")

initialWorld :: World
initialWorld = mkWorld [e1, e2, e3]

---- SYSTEMS ----

move1 :: World -> World
move1 = mapW (\(Position x y) -> Position (x + 1) y)

renameEnemy :: World -> World
renameEnemy = doubleQW f
  where
    f (Player, Position x y) (Name _) = Name $ "player is at: " ++ show (x, y)
    f (Player, Position x y) (Name2 _) = Name2 $ "player is at: " ++ show (x, y)

removePosOfEnt2 :: Name -> Maybe (Remove Position)
removePosOfEnt2 (Name2 "Entity2") = Just Remove
removePosOfEnt2 _ = Nothing

spawnClone :: Name -> SpawnEntity
spawnClone (Name2 "Entity2") = Just $ mkEntity >:> (Name2 "Clone")
spawnClone _ = Nothing



---- MAIN ----
main :: IO ()
main = do
  let ecs = mkECS initialWorld [move1, mapW removePosOfEnt2 , mapW spawnClone, renameEnemy]
  printState ecs
  let ecs' = step ecs
  printState ecs'
  return ()

