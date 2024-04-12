{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

module Types where

import qualified Data.Map as M
import Type.Reflection (SomeTypeRep, Typeable)

-- | Entity name
type EName = Int

-- | Entity is a list of components
newtype Entity = E (M.Map SomeTypeRep Component)

-- | Component is a wrapper around some data
data Component where
  C :: (Typeable a, Show a) => a -> Component

instance Show Entity where
  show (E e) =
    let width = maximum $ map (length . show) $ M.keys e
     in unlines $ map (\(k, v) -> show k ++ replicate (width - length (show k)) ' ' ++ " : " ++ show v) $ M.toList e

instance Show Component where
  show (C c) = show c

-- | World is a map of entities
data World = World
  { entities :: M.Map EName Entity
  }

-- | ECS is the main data structure
data ECS = ECS
  { world :: World,
    systems :: [World -> World]
  }

-- | IsComponent is a dummy typeclass
class IsComponent a

