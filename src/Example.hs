module Example where

import Data.Data (Proxy (..), Typeable)
import Functions
  ( Entity,
    World,
    mapW,
    mkECS,
    mkEntity,
    mkWorld,
    printECS,
    step,
    (>:>),
  )

---- COMPONENTS ----

-- | Position component
data Position = Position
  { pos_x :: Int,
    pos_y :: Int
  }
  deriving (Show, Eq, Typeable)

-- | Name component
data Name
  = Name
      { name :: String
      }
  | Name2
      { name :: String
      }
  deriving (Show, Eq, Typeable)

---- CONSTRUCTION ----

e1 :: Entity
e1 = mkEntity >:> (Position 0 0) >:> (Name "Entity")

e2 :: Entity
e2 = mkEntity >:> (Position 1 1) >:> (Name2 "Entity2")

initialWorld :: World
initialWorld = mkWorld [e1, e2]

---- SYSTEMS ----

move1 :: World -> World
move1 = mapW ((Proxy :: Proxy Position)) (\(Position x y) -> Position (x + 1) y)

---- MAIN ----
main :: IO ()
main = do
  let ecs = mkECS initialWorld [move1]
  printECS ecs
  let ecs' = step ecs
  printECS ecs'
  return ()