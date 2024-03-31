module Functions
  ( -- * World functions
    mkWorld,
    mkECS,
    step,
    printECS,
    -- \^ DEBUG: print the ECS

    -- * Entity functions
    mkEntity,
    (>:>),

    -- * System functions
    mapW,
    mapWIO,

    -- * Types (re-exported for convenience)
    World,
    ECS,
    Entity,
    Query (..),
  )
where

import qualified Data.Map as M
import Data.Typeable (Typeable, cast, typeOf, typeRep)
import Types
  ( Component (C),
    ComponentData (..),
    ECS (ECS),
    EName,
    Entity (..),
    Query (..),
    World (World),
  )

---------- WORLD FUNCTIONS --------------

-- | Create a world from a list of entities
mkWorld :: [Entity] -> World
mkWorld es = World $ M.fromList $ zip [1 ..] es

-- | Create an ECS from a world and a list of systems
mkECS :: World -> [World -> World] -> ECS
mkECS w ss = ECS w ss

-- | Step the ECS one tick
step :: ECS -> ECS
step (ECS w ss) = ECS (runStep ss w) ss

-- | Print the ECS
printECS :: ECS -> IO ()
printECS (ECS w _) = printWorld w

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
mapW :: (Typeable a, Show a) => Query a -> (a -> a) -> World -> World
mapW q f (World e) = World $ M.map (modifyComponent q f) e

-- | Map an IO function over a component in the world
mapWIO :: (Typeable a, Show a) => Query a -> (a -> IO ()) -> World -> IO ()
mapWIO q f (World e) = mapM_ (f . (\(CD c) -> c)) $ M.elems $ M.mapMaybe (lookupComponent q) e

-- | Run a list of functions over the world
runStep :: [World -> World] -> World -> World
runStep [] w = w
runStep (s : ss) w = runStep ss (s w)

---------- COMPONENT FUNCTIONS ----------

-- | Modify a component in an entity
modifyComponent :: (Typeable a) => Query a -> (a -> a) -> Entity -> Entity
modifyComponent q f e@(E en) = case lookupComponent q e of
  Just (CD c) -> E $ M.insert (typeRep q) (C $ CD (f c)) en
  Nothing -> e

-- | Lookup a component in an entity
lookupComponent :: (Typeable a) => Query a -> Entity -> Maybe (ComponentData a)
lookupComponent q (E e) = case M.lookup (typeRep q) e of
  Just (C c) -> cast c
  Nothing -> Nothing
