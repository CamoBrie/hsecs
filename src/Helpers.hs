module Helpers where
import Type.Reflection (Typeable, TypeRep, someTypeRep)
import Types (Entity (E), ComponentData (CD), Component (C))
import qualified Data.Map as M
import Data.Typeable (cast)

-- | look up a component type in an entity
lookupComponent :: (Typeable a) => TypeRep a -> Entity -> Maybe a
lookupComponent t (E e) = do 
    (C (CD x)) <- M.lookup (someTypeRep t) e
    x' <- cast x
    return x'

