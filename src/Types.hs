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
import Debug.Trace
import GHC.Generics

type EName = Int

type Entity = [Component]

data Component where
  C :: (Typeable a, Show a) => ComponentData a -> Component

data ComponentData a where
  CD :: (Typeable a, Show a) => a -> ComponentData a

data Query a where
  Q :: (Typeable a, Show a) => ComponentData a -> Query a

instance Show Component where
  show (C c) = show c

instance Show (ComponentData a) where
  show (CD c) = show c

data Position = Position
  { pos_x :: Double,
    pos_y :: Double
  }
  deriving (Show, Eq, Data, Typeable, Generic)

data Name = Name
  { name :: String
  }
  deriving (Show, Eq, Data, Typeable, Generic)

data World = World
  { entities :: M.Map EName Entity
  }

---------- DEBUGGING FUNCTIONS ----------

printWorld :: World -> IO ()
printWorld (World e) = do
  putStrLn "World:"
  mapM_ printEntity (M.toList e)

printEntity :: (EName, [Component]) -> IO ()
printEntity (n, entity) = do
  putStrLn $ "Entity " ++ show n ++ ":"
  mapM_ printComponent entity

printComponent :: Component -> IO ()
printComponent (C (CD c)) = print c

mkEntity :: Entity
mkEntity = [C $ CD (Position 0 0), C $ CD (Name "Entity")]

mkEntity2 :: Entity
mkEntity2 = [C $ CD (Position 1 1), C $ CD (Name "Entity2")]

mkWorld :: World
mkWorld = World $ M.fromList [(1, mkEntity), (2, mkEntity2)]

move1 :: World -> World
move1 (World e) = World $ M.map (moveEntity 1) e

moveEntity :: Double -> Entity -> Entity
moveEntity dx entity = case lookupComponent entity (Q $ CD (Position 0 0)) of
  Just (CD (Position x y)) -> replaceComponent entity (C $ CD (Position (x + dx) y))
  Nothing -> entity

---------- COMPONENT FUNCTIONS ----------
lookupComponent :: (Typeable a) => Entity -> Query a -> Maybe (ComponentData a)
lookupComponent [] _ = Nothing
lookupComponent (C c : cs) (Q q)
  | typeOf c == typeOf q = cast c
  | otherwise = lookupComponent cs (Q q)

replaceComponent :: Entity -> Component -> Entity
replaceComponent [] _ = []
replaceComponent (C c : cs) (C c')
  | typeOf c == typeOf c' = C c' : cs
  | otherwise = C c : replaceComponent cs (C c')