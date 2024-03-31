module Functions where

import qualified Data.Map as M
import Data.Typeable
import GHC.Generics
import Types

---------- WORLD FUNCTIONS --------------
mkWorld :: [Entity] -> World
mkWorld es = World $ M.fromList $ zip [1 ..] es

---------- ENTITY FUNCTIONS -------------

-- | Create an entity
mkEntity :: Entity
mkEntity = E M.empty

-- | Add a component to an entity
(>:>) :: (Typeable a, Show a, Generic a) => Entity -> a -> Entity
(>:>) (E e) c = E $ M.insert (typeOf c) (C $ CD c) e

infixl 5 >:>

---------- SYSTEM FUNCTIONS -------------

-- | Map a function over a component in the world
mapW :: (Typeable a, Show a) => Proxy a -> (a -> a) -> World -> World
mapW q f (World e) = World $ M.map (modifyComponent (typeRep q) f) e

-- | Map an IO function over a component in the world
mapWIO :: (Typeable a, Show a) => Proxy a -> (a -> IO ()) -> World -> IO ()
mapWIO q f (World e) = mapM_ (f . (\(CD c) -> c)) $ M.elems $ M.mapMaybe (lookupComponent (typeRep q)) e

-- | Run a list of functions over the world
runStep :: [World -> World] -> World -> World
runStep [] w = w
runStep (s : ss) w = runStep ss (s w)

---------- COMPONENT FUNCTIONS ----------

-- | Modify a component in an entity
modifyComponent :: (Typeable a) => Query a -> (a -> a) -> Entity -> Entity
modifyComponent q f e@(E en) = case lookupComponent q e of
  Just (CD c) -> E $ M.insert q (C $ CD (f c)) en
  Nothing -> e

-- | Lookup a component in an entity
lookupComponent :: (Typeable a) => Query a -> Entity -> Maybe (ComponentData a)
lookupComponent (t) (E e) = case M.lookup t e of
  Just (C c) -> cast c
  Nothing -> Nothing