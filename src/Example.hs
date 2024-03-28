{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Example where

import Data.Data
import qualified Data.Map as M
import Functions
import GHC.Generics
import Types

-- | Position component
data Position = Position
  { pos_x :: Int,
    pos_y :: Int
  }
  deriving (Show, Eq, Data, Typeable, Generic)

-- | Name component
data Name = Name
  { name :: String
  }
  deriving (Show, Eq, Data, Typeable, Generic)

---------- DEBUGGING FUNCTIONS ----------

printWorld :: World -> IO ()
printWorld (World e) = do
  mapM_ printEntity (M.toList e)

printEntity :: (EName, Entity) -> IO ()
printEntity (n, entity) = do
  putStrLn $ "\n-----Entity " ++ show n ++ "-----"
  mapM_ printComponent entity

printComponent :: Component -> IO ()
printComponent (C (CD c)) = print c

mkEntity :: Entity
mkEntity = M.fromList [(typeOf Position, (C $ CD (Position 0 0))), (typeOf Name, (C $ CD (Name "Entity")))]

mkEntity2 :: Entity
mkEntity2 = M.fromList [(typeOf Position, (C $ CD (Position 1 1))), (typeOf Name, (C $ CD (Name "Entity2")))]

mkWorld :: World
mkWorld = World $ M.fromList [(1, mkEntity), (2, mkEntity2)]

move1 :: World -> World
move1 = mapW (typeOf Position) (\(Position x y) -> Position (x + 1) y)

-- go :: World -> World
-- go = runStep [move1, move1, move1]
