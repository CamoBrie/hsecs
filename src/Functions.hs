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

    -- * Types (re-exported for convenience)
    World,
    ECS,
    Entity,
  )
where

import Classes (ComponentEffect (modifyEntity), IsComponent, Queryable (performQuery), SystemResult (applyEffect))
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Typeable (Typeable, typeOf)
import Helpers (lookupComponent)
import Type.Reflection (TypeRep, (:~:) (Refl), pattern Fun)
import qualified Type.Reflection as R
import Types
  ( Component (C),
    ComponentData (..),
    ECS (ECS),
    EName,
    Entity (..),
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
(>:>) (E e) c = E $ M.insert (typeOf c) (C $ CD c) e

infixl 5 >:>

---------- SYSTEM FUNCTIONS -------------

-- | Run a list of functions over the world
runStep :: [World -> World] -> World -> World
runStep [] w = w
runStep (s : ss) w = runStep ss (s w)

-- | Collect the results of a function over the ECS
collectW :: (Queryable a, Typeable b, SystemResult b) => (a -> b) -> (ECS -> [b])
collectW f (ECS w _) = map f $ catMaybes $ map (lookupComponent qs) $ map snd $ getEntities qs w
  where
    (qs, _) = splitSystem Refl (R.typeOf f)

---------- COMPONENT FUNCTIONS ----------

-- | convert a function to a system that can be run in the ECS
mapW :: (Queryable a, Typeable b, SystemResult b) => (a -> b) -> (World -> World)
mapW f = applyEffect $ \e -> f <$> (performQuery qs) e
  where
    (qs, _) = splitSystem Refl (R.typeOf f)

-- | Convert a predicate function to a system that can be run in the ECS
filterW :: (Queryable a) => (a -> Bool) -> (World -> World)
filterW f (World e) = World $ M.filter (filterE f qf) e
  where
    (qs, _) = splitSystem Refl (R.typeOf f)
    qf = performQuery qs

filterE :: (a -> Bool) -> (Entity -> Maybe a) -> (Entity -> Bool)
filterE f qf e = fromMaybe False $ do
  q <- qf e
  return $ f q

-- | Combined query and modify function for a system that can be run in the ECS.
-- The left query entity has to be only 1 entity.
-- The right query entity will be updated. The left entity will be excluded from the query.
doubleQW :: (Queryable a, Queryable b, ComponentEffect c) => (a -> b -> c) -> (World -> World)
doubleQW f w@(World e) = World $ M.mapWithKey (doubleQ name f as1 qf1 modifyEntity) e
  where
    (qa, f') = splitSystem Refl (R.typeOf f)
    (qb, _) = splitSystem Refl f'
    (name, as1) = getUniqueComponent qa w
    qf1 = performQuery qb

doubleQ :: EName -> (a -> b -> c) -> a -> (Entity -> Maybe b) -> (c -> Entity -> Entity) -> EName -> Entity -> Entity
doubleQ name f es1 qf mf n e
  | name == n = e -- exclude the entity from the query
  | otherwise = fromMaybe e $ do
      q1 <- qf e
      let res = f es1 q1
      return $ mf res e

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
getUniqueComponent :: (Queryable a) => TypeRep a -> World -> (EName, a)
getUniqueComponent a (World e) = case getEntities a (World e) of
  [(n, e')] -> case performQuery a e' of
    Just a' -> (n, a')
    _ -> error "Expected the component, found nothing"
  xs -> error $ "Expected exactly one entity that passes the query, found: " ++ show xs
