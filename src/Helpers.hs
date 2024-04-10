module Helpers where

import qualified Data.Map as M
import Data.Typeable (cast)
import Type.Reflection (TypeRep, Typeable, someTypeRep)
import Types (Component (C), ComponentData (CD), Entity (E))

-- | look up a component type in an entity
lookupComponent :: (Typeable a) => TypeRep a -> Entity -> Maybe a
lookupComponent t (E e) = do
  (C (CD x)) <- M.lookup (someTypeRep t) e
  x' <- cast x
  return x'
