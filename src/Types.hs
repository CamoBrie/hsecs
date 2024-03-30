{-# LANGUAGE GADTs #-}

module Types where

import Data.Data
import qualified Data.Map as M
import GHC.Generics
import Type.Reflection

-- | Entity name
type EName = Int

-- | Entity is a list of components
newtype Entity = E (M.Map SomeTypeRep Component)

-- | Component is a wrapper around some data
data Component where
  C :: (Typeable a, Show a, Generic a) => ComponentData a -> Component

-- | ComponentData is the actual data stored in a component
data ComponentData a where
  CD :: (Typeable a, Show a, Generic a) => a -> ComponentData a

-- | Query is a wrapper around querying for a specific type of component
type Query a = SomeTypeRep

instance Show Entity where
  show (E e) =
    let width = maximum $ map (length . show) $ M.keys e
     in unlines $ map (\(k, v) -> show k ++ replicate (width - length (show k)) ' ' ++ " : " ++ show v) $ M.toList e

instance Show Component where
  show (C c) = show c

instance Show (ComponentData a) where
  show (CD c) = show c

-- | World is a map of entities
data World = World
  { entities :: M.Map EName Entity
  }
