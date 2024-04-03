{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module QueryGen where

import Type.Reflection
import Data.Typeable (cast)
import Data.Map as M
import Types (Entity (..), ComponentData (..), Component (..))

splitSystem :: (Typeable a, Typeable b, Typeable c) => (a :~: (b -> c)) -> TypeRep a -> (TypeRep b, TypeRep c)
splitSystem Refl (Fun args results) = (args, results)

lookupSafe :: (Typeable a) => TypeRep a -> Entity -> Maybe a
lookupSafe t (E e) = do 
    (C (CD x)) <- M.lookup (someTypeRep t) e
    x' <- cast x
    return x'

class (Typeable a) => Queryable a where
    performQuery :: TypeRep a -> Entity -> Maybe a
    
instance {-# OVERLAPPABLE #-} (Typeable a) => Queryable a where
    performQuery t e = lookupSafe t e

instance (Queryable a, Queryable b) => (Queryable (a, b)) where
    performQuery (App (App _ t1) t2) e = do
        v1 <- lookupSafe t1 e
        v2 <- lookupSafe t2 e
        return (v1, v2)
