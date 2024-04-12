{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

module SystemResults where

import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)
import Type.Reflection (Typeable, someTypeRep, splitApps, typeOf)
import Types (Component (C), EName, Entity (E), IsComponent, World (World))

-- | SystemResults speficy types that can be the result of a system
class SystemResult a where
  applyEffect :: (EName -> Entity -> Maybe a) -> World -> World

-- | ComponentEffects change the world by updating the speficied components of that entity
instance (ComponentEffect a) => SystemResult a where
  applyEffect q (World w) = World $ M.mapWithKey f w
    where
      f :: EName -> Entity -> Entity
      f n e = fromMaybe e $ do
        r <- q n e
        return $ modifyEntity r e

-- | ComponentEffects change component values
class (Show a, Typeable a) => ComponentEffect a where
  modifyEntity :: a -> Entity -> Entity

instance {-# OVERLAPPABLE #-} (Show a, Typeable a, IsComponent a) => ComponentEffect a where
  modifyEntity a (E e) = E $ M.insert (someTypeRep $ typeOf a) (C a) e

instance {-# OVERLAPPABLE #-} (Show a, Typeable a, IsComponent a) => ComponentEffect (Maybe a) where
  modifyEntity (Just x) e = modifyEntity x e
  modifyEntity Nothing e = e

instance (ComponentEffect a, ComponentEffect b) => ComponentEffect (a, b) where
  modifyEntity (a, b) e = modifyEntity b $ modifyEntity a e

instance (ComponentEffect a, ComponentEffect b, ComponentEffect c) => ComponentEffect (a, b, c) where
  modifyEntity (a, b, c) e = modifyEntity (b, c) $ modifyEntity a e

instance (ComponentEffect a, ComponentEffect b, ComponentEffect c, ComponentEffect d) => ComponentEffect (a, b, c, d) where
  modifyEntity (a, b, c, d) e = modifyEntity (b, c, d) $ modifyEntity a e

type SpawnEntity = Maybe Entity

instance {-# OVERLAPPING #-} SystemResult SpawnEntity where
  applyEffect q (World w) = World $ foldr f w $ zip spawnedEs [(M.size w + 1) ..]
    where
      spawnedEs = catMaybes $ catMaybes $ map (uncurry q) $ M.toList w
      f (ent, i) m = M.insert i ent m

data Remove a = Remove
  deriving (Show)

instance (Show a, Typeable a, IsComponent a) => ComponentEffect (Remove a) where
  modifyEntity x (E e) = case splitApps $ typeOf x of
    (_, [v]) -> E $ M.delete v e
    _ -> error "unreachable, Remove is always applied to one type"

instance (Show a, Typeable a, IsComponent a) => ComponentEffect (Maybe (Remove a)) where
  modifyEntity (Just x) e = modifyEntity x e
  modifyEntity Nothing e = e
