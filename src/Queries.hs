{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module Queries where
import Type.Reflection (Typeable, TypeRep, pattern App)
import Types (Entity, IsComponent)
import Helpers (lookupComponent)

-- | Queryable types can be used as input for systems
class (Typeable a) => Queryable a where
  performQuery :: TypeRep a -> Entity -> Maybe a

instance {-# OVERLAPPABLE #-} (IsComponent a, Typeable a) => Queryable a where
  performQuery t e = lookupComponent t e

instance (Queryable a, Queryable b) => (Queryable (a, b)) where
  performQuery (App (App _ t1) t2) e = do
    v1 <- lookupComponent t1 e
    v2 <- lookupComponent t2 e
    return (v1, v2)

instance (Queryable a, Queryable b, Queryable c) => (Queryable (a, b, c)) where
  performQuery (App (App (App _ t1) t2) t3) e = do
    v1 <- lookupComponent t1 e
    v2 <- lookupComponent t2 e
    v3 <- lookupComponent t3 e
    return (v1, v2, v3)

instance (Queryable a, Queryable b, Queryable c, Queryable d) => (Queryable (a, b, c, d)) where
  performQuery (App (App (App (App _ t1) t2) t3) t4) e = do
    v1 <- lookupComponent t1 e
    v2 <- lookupComponent t2 e
    v3 <- lookupComponent t3 e
    v4 <- lookupComponent t4 e
    return (v1, v2, v3, v4)
