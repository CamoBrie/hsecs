module Functions where

import qualified Data.Map as M
import Data.Typeable
import Types

---------- QUERY FUNCTIONS ----------
mkQuery :: (Typeable a) => Proxy a -> Query a
mkQuery = typeRep

---------- SYSTEM FUNCTIONS -------------

-- | Map a function over a component in the world
mapW :: (Typeable a, Show a) => Proxy a -> (a -> a) -> World -> World
mapW q f (World e) = World $ M.map (modifyComponent (mkQuery q) f) e

-- | Map an IO function over a component in the world
mapWIO :: (Typeable a, Show a) => Proxy a -> (a -> IO ()) -> World -> IO ()
mapWIO q f (World e) = mapM_ (f . (\(CD c) -> c)) $ M.elems $ M.mapMaybe (lookupComponent (mkQuery q)) e

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