{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}

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
    filterW,
    doubleQW,
    collectW,
    runSys,

    -- * Types (re-exported for convenience)
    World,
    ECS,
    Entity,
    IsComponent,
  )
where

import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Typeable (Typeable, typeOf)
import Queries (Queryable (performQuery))
import SystemResults (SystemResult (applyEffect))
import Type.Reflection (TypeRep, (:~:) (Refl), pattern Fun)
import qualified Type.Reflection as R
import Types
  ( Component (C),
    ECS (ECS),
    EName,
    Entity (..),
    IsComponent,
    World (World),
  )

---------- WORLD FUNCTIONS --------------

-- | Create a world from a list of entities
mkWorld :: [Entity] -> World
mkWorld es = World $ M.fromList $ zip [1 ..] es

-- | Create an ECS from a world and a list of systems
mkECS :: World -> [World -> World] -> ECS
mkECS w ss = ECS w ss

-- | Print the state of the ECS
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
(>:>) :: (IsComponent a, Typeable a, Show a) => Entity -> a -> Entity
(>:>) (E e) c = E $ M.insert (typeOf c) (C c) e

infixl 5 >:>

---------- SYSTEM FUNCTIONS -------------

-- | Run a list of functions over the world
runStep :: [World -> World] -> World -> World
runStep [] w = w
runStep (s : ss) w = runStep ss (s w)

-- | Collect the results of a function over the ECS
collectW :: (Queryable a, Typeable b) => (a -> b) -> (ECS -> [b])
collectW f (ECS w _) = map f $ catMaybes $ map (performQuery qs) $ map snd $ getEntities qs w
  where
    (qs, _) = splitSystem Refl (R.typeOf f)

-- | Run a system directly on the ECS
runSys :: (World -> World) -> ECS -> ECS
runSys s (ECS w ss) = ECS (s w) ss

---------- COMPONENT FUNCTIONS ----------

-- | convert a function to a system that can be run in the ECS
mapW :: (Queryable a, Typeable b, SystemResult b) => (a -> b) -> (World -> World)
mapW f = applyEffect $ \_ e -> f <$> (performQuery qs) e
  where
    (qs, _) = splitSystem Refl (R.typeOf f)

-- | Convert a predicate function to a system that can be run in the ECS
filterW :: (Queryable a) => (a -> Bool) -> (World -> World)
filterW f (World e) = World $ M.filter (filterE f qf) e
  where
    (qs, _) = splitSystem Refl (R.typeOf f)
    qf = performQuery qs

filterE :: (a -> Bool) -> (Entity -> Maybe a) -> (Entity -> Bool)
filterE f qf e = fromMaybe True $ do
  q <- qf e
  return $ f q

-- | Combined query and modify function for a system that can be run in the ECS.
-- The left query entity has to be only 1 entity.
-- The right query entity will be updated. The left entity will be excluded from the query.
doubleQW :: (Queryable a, Queryable b, Typeable c, SystemResult c) => (a -> b -> c) -> (World -> World)
doubleQW f w = case getUniqueComponent qs1 w of
  Nothing -> w
  Just (name, as1) -> applyEffect (q name as1) w
  where
    q name as1 name' e
      | name' == name = Nothing
      | otherwise = do
          es1 <- performQuery qs2 e
          return $ f as1 es1

    (qs1, f') = splitSystem Refl (R.typeOf f)
    (qs2, _) = splitSystem Refl f'

-------- UTILITY FUNCTIONS --------------

-- | Split a function type into its argument and result types
splitSystem ::
  (Typeable a, Typeable b, Typeable c) =>
  (a :~: (b -> c)) ->
  TypeRep a ->
  (TypeRep b, TypeRep c)
splitSystem Refl (Fun args results) = (args, results)
splitSystem Refl _ = error "impossible"

-- | Get all entities that pass the query
getEntities :: (Queryable a) => TypeRep a -> World -> [(EName, Entity)]
getEntities a (World e) = M.toList $ M.filter (isJust . performQuery a) $ e

-- | Get the unique entity that passes the query
getUniqueComponent :: (Queryable a) => TypeRep a -> World -> Maybe (EName, a)
getUniqueComponent a (World e) = case getEntities a (World e) of
  [(n, e')] -> case performQuery a e' of
    Just a' -> Just (n, a')
    _ -> Nothing
  _ -> Nothing
