module Helpers where

import qualified Data.Map as M
import Data.Typeable (cast)
import Type.Reflection (TypeRep, Typeable, someTypeRep)
import Types (Component (C), Entity (E))

-- | look up a component type in an entity
lookupComponent :: (Typeable a) => TypeRep a -> Entity -> Maybe a
lookupComponent t (E e) = do
  (C x) <- M.lookup (someTypeRep t) e
  cast x