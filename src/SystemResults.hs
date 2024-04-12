{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module SystemResults where
import Types (Entity (E), World (World), IsComponent, Component (C), ComponentData (CD))
import Type.Reflection (Typeable, someTypeRep, typeOf)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, catMaybes)

-- | SystemResults speficy types that can be the result of a system
class SystemResult a where
  applyEffect :: (Entity -> Maybe a) -> World -> World

-- | ComponentEffects change the world by updating the speficied components of that entity
instance ComponentEffect a => SystemResult a where
  applyEffect q (World w) = World $ M.map f w
    where
      f :: Entity -> Entity
      f e = fromMaybe e $ do
        r <- q e
        return $ modifyEntity r e

-- | ComponentEffects change component values
class (Show a, Typeable a) => ComponentEffect a where
  modifyEntity :: a -> Entity -> Entity

instance {-# OVERLAPPABLE #-} (Show a, Typeable a, IsComponent a) => ComponentEffect a where
  modifyEntity a (E e) = E $ M.insert (someTypeRep $ typeOf a) (C $ CD a) e

instance (ComponentEffect a, ComponentEffect b) => ComponentEffect (a, b) where
  modifyEntity (a, b) e = modifyEntity b $ modifyEntity a e

instance (ComponentEffect a, ComponentEffect b, ComponentEffect c) => ComponentEffect (a, b, c) where
  modifyEntity (a, b, c) e = modifyEntity (b, c) $ modifyEntity a e

instance (ComponentEffect a, ComponentEffect b, ComponentEffect c, ComponentEffect d) => ComponentEffect (a, b, c, d) where
  modifyEntity (a, b, c, d) e = modifyEntity (b, c, d) $ modifyEntity a e

type SpawnEntity = Maybe Entity 

instance {-# OVERLAPPING #-} SystemResult SpawnEntity where
    applyEffect q (World w) = World $ foldr f w $ zip spawnedEs [(M.size w +1)..]
        where 
            spawnedEs = catMaybes $ catMaybes $ map q $ M.elems w
            f (ent, i) m = M.insert i ent m
