{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
module Functions
  ( -- * World functions
    mkWorld,
    mkECS,
    printState,
    step,
    -- \^ DEBUG: print the ECS

    -- * Entity functions
    mkEntity,
    (>:>),

    -- * System functions
    mapW,

    -- * Types (re-exported for convenience)
    World,
    ECS,
    Entity,
  )
where

import qualified Data.Map as M
import Data.Typeable (Typeable, typeOf)
import qualified Type.Reflection as R
import Types
  ( Component (C),
    ComponentData (..),
    ECS (ECS),
    EName,
    Entity (..),
    World (World),
  )
import Classes (Queryable (performQuery), SystemResult (modifyEntity))
import Type.Reflection ((:~:) (Refl), TypeRep, pattern Fun)
import Data.Maybe (fromMaybe)

---------- WORLD FUNCTIONS --------------

-- | Create a world from a list of entities
mkWorld :: [Entity] -> World
mkWorld es = World $ M.fromList $ zip [1 ..] es

-- | Create an ECS from a world and a list of systems
mkECS :: World -> [World -> World] -> ECS
mkECS w ss = ECS w ss

printState :: ECS -> IO ()
printState (ECS w ss) = do
    printWorld w
    putStrLn $ "There are " ++ show (length ss) ++ " systems registered."

-- | Step the ECS one tick
step :: ECS -> ECS
step (ECS w ss) = ECS (runStep ss w) ss

printWorld :: World -> IO ()
printWorld (World e) = do
  mapM_ printEntity (M.toList e)

printEntity :: (EName, Entity) -> IO ()
printEntity (n, e) = do
  putStrLn $ "\n-----Entity " ++ show n ++ "-----"
  print e

---------- ENTITY FUNCTIONS -------------

-- | Create an entity
mkEntity :: Entity
mkEntity = E M.empty

-- | Add a component to an entity
(>:>) :: (Typeable a, Show a) => Entity -> a -> Entity
(>:>) (E e) c = E $ M.insert (typeOf c) (C $ CD c) e

infixl 5 >:>

---------- SYSTEM FUNCTIONS -------------

-- | Map a function over a component in the world

-- | Run a list of functions over the world
runStep :: [World -> World] -> World -> World
runStep [] w = w
runStep (s : ss) w = runStep ss (s w)

---------- COMPONENT FUNCTIONS ----------

-- | convert a function to a system that can be run in the ECS
mapW :: (Queryable a, SystemResult b) => (a -> b) -> (World -> World)
mapW f (World e) = World $ M.map (mapE f qf modifyEntity) e
    where 
        (qs, _) = splitSystem Refl (R.typeOf f)

        qf = performQuery qs

-- Helpes \/

mapE :: (a -> b) -> (Entity -> Maybe a) -> (b -> Entity -> Entity) -> (Entity -> Entity)
mapE f qf mf e = fromMaybe e $ do 
    q <- qf e
    let res = f q
    return $ mf res e

splitSystem :: (Typeable a, Typeable b, Typeable c) 
            => (a :~: (b -> c)) 
            -> TypeRep a 
            -> (TypeRep b, TypeRep c)
splitSystem Refl (Fun args results) = (args, results)
splitSystem Refl _ = error "impossible"

