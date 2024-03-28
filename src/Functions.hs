module Functions where

import qualified Data.Map as M
import Data.Typeable
import Types

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