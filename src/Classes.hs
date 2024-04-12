{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

module Classes where

import qualified Data.Map as M
import Helpers (lookupComponent)
import Type.Reflection (TypeRep, Typeable, someTypeRep, typeOf, pattern App)
import Types (Component (..), ComponentData (..), Entity (E), World (World))
import Data.Maybe (fromMaybe)

-- | Queryable types can be used as input for systems
class (Typeable a) => Queryable a where
  performQuery :: TypeRep a -> Entity -> Maybe a

instance {-# OVERLAPPABLE #-} (Typeable a) => Queryable a where
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

class SystemResult a where
  applyEffect :: (Entity -> Maybe a) -> World -> World

instance ComponentEffect a => SystemResult a where
  applyEffect q (World w) = World $ M.map f w
    where 
        f :: Entity -> Entity
        f e = fromMaybe e $ do
            r <- q e
            return $ modifyEntity r e

-- | ComponentEffect can be used as the result of a system.
class (Show a, Typeable a) => ComponentEffect a where
  modifyEntity :: a -> Entity -> Entity

instance {-# OVERLAPPABLE #-} (Show a, Typeable a) => ComponentEffect a where
  modifyEntity a (E e) = E $ M.insert (someTypeRep $ typeOf a) (C $ CD a) e

instance (ComponentEffect a, ComponentEffect b) => ComponentEffect (a, b) where
  modifyEntity (a, b) e = modifyEntity b $ modifyEntity a e

instance (ComponentEffect a, ComponentEffect b, ComponentEffect c) => ComponentEffect (a, b, c) where
  modifyEntity (a, b, c) e = modifyEntity (b, c) $ modifyEntity a e

instance (ComponentEffect a, ComponentEffect b, ComponentEffect c, ComponentEffect d) => ComponentEffect (a, b, c, d) where
  modifyEntity (a, b, c, d) e = modifyEntity (b, c, d) $ modifyEntity a e
