{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module Queries where
import Type.Reflection (Typeable, TypeRep, pattern App, splitApps)
import Types (Entity (E), IsComponent)
import Helpers (lookupComponent)
import qualified Data.Map as M

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

data Not a = Not

instance Queryable a => Queryable (Not a) where
    performQuery n (E e) = case splitApps n of
        (_, [q]) -> case M.lookup q e of
            (Just _) -> Nothing
            Nothing -> Just Not
        _ -> error "unreachable Not is always applied to exactly one type"
