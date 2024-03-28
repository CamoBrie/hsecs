{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Types where

import Data.Data
import qualified Data.Map as M
import GHC.Generics

-- | Entity name
type EName = Int

-- | Entity is a list of components
type Entity = M.Map TypeRep Component

-- | Component is a wrapper around some data
data Component where
  C :: (Typeable a, Show a, Generic a) => ComponentData a -> Component

-- | ComponentData is the actual data stored in a component
data ComponentData a where
  CD :: (Typeable a, Show a, Generic a) => a -> ComponentData a

-- | Query is a wrapper around querying for a specific type of component
type Query a = TypeRep

instance Show Component where
  show (C c) = show c

instance Show (ComponentData a) where
  show (CD c) = show c

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

-- | World is a map of entities
data World = World
  { entities :: M.Map EName Entity
  }

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

---------- QUERY FUNCTIONS ----------

---------- SYSTEM FUNCTIONS -------------
mapW :: (Typeable a, Show a) => Query a -> (a -> a) -> World -> World
mapW q f (World e) = World $ M.map (modifyComponent q f) e

runStep :: [World -> World] -> World -> World
runStep [] w = w
runStep (s : ss) w = runStep ss (s w)

---------- COMPONENT FUNCTIONS ----------
modifyComponent :: (Typeable a) => Query a -> (a -> a) -> Entity -> Entity
modifyComponent q f e = case lookupComponent q e of
  Just (CD c) -> replaceComponent e q (C $ CD (f c))
  Nothing -> e

lookupComponent :: (Typeable a) => Query a -> Entity -> Maybe (ComponentData a)
lookupComponent (t) e = case M.lookup t e of
  Just (C c) -> cast c
  Nothing -> Nothing

replaceComponent :: Entity -> Query a -> Component -> Entity
replaceComponent e q (C (CD c)) = M.insert q (C $ CD c) e