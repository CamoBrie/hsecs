{-# LANGUAGE DeriveAnyClass #-}
module Main (main) where

import Data.Maybe (isJust)
import qualified Data.Map as M
import Functions
import Helpers
import Test.Tasty
import Test.Tasty.QuickCheck
import Type.Reflection
import Functions (IsComponent, mkEntity, mapW, World, step)
import Types
import Queries (Queryable)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Main"
    [   testProperty "Component Result can create new component" singleComponentEffectCreates
    ,   testProperty "Double Component Result can create new components" doubleComponentEffectCreates
    ,   testProperty "Single Component Result updates if present" singleComponentEffectUpdates
    ,   testProperty "Double Component Result update if present" doubleComponentEffectUpdates
    ,   testProperty "Not Queries those without the component" notOnlyMatchesThoseWithout
    ,   testProperty "Not Queries those without the component, even when nothing matches" notCanMatchNothing
    ,   testProperty "Tuple queries match and wise" queryAnd
    ]

singleComponentEffectCreates :: Bool
singleComponentEffectCreates = run [entA] (mapW $ \A -> C1) `equiv` [entAC1]

doubleComponentEffectCreates :: Bool
doubleComponentEffectCreates = run [entA] (mapW $ \A -> (B True, C1)) `equiv` [entABTC1]

singleComponentEffectUpdates :: Bool
singleComponentEffectUpdates = run [entAC1] (mapW $ \A -> C2) `equiv` [entAC2]

doubleComponentEffectUpdates :: Bool
doubleComponentEffectUpdates = run [entABTC1] (mapW $ \A -> (B False, C2)) `equiv` [entABFC2]

notOnlyMatchesThoseWithout :: Bool
notOnlyMatchesThoseWithout = run [entA, entABT] (mapW sys) `equiv` [entABF, entABT]
    where sys :: Not B -> B
          sys _ = B False

notCanMatchNothing :: Bool
notCanMatchNothing  = run [entABF, entABT] (mapW sys) `equiv` [entABF, entABT]
    where sys :: Not B -> C
          sys _ = C2

queryAnd :: Bool
queryAnd = run [entA, entABT] (mapW $ \(A, (B _)) -> C1) `equiv` [entA, entABTC1]


entA = mkEntity >:> A
entABT = mkEntity >:> A >:> B True
entABF = mkEntity >:> A >:> B False
entAC1 = mkEntity >:> A >:> C1
entAC2 = mkEntity >:> A >:> C2
entABTC1 = mkEntity >:> A >:> B True >:> C1
entABTC2 = mkEntity >:> A >:> B True >:> C2
entABFC1 = mkEntity >:> A >:> B False >:> C1
entABFC2 = mkEntity >:> A >:> B False >:> C2

data A = A
    deriving(IsComponent, Show, Typeable, Eq)
data B = B Bool
    deriving(IsComponent, Show, Typeable, Eq)
data C = C1 | C2
    deriving(IsComponent, Show, Typeable, Eq)

run :: [Entity] -> (World -> World) -> [Entity]
run es s = 
    let ecs = mkECS (mkWorld es) [s] in
    let ecs' = step ecs in 
    M.elems $ entities $ world $ ecs'

equiv :: [Entity] -> [Entity] -> Bool
equiv e1s e2s = and $ zipWith compareEntities e1s e2s

compareEntities :: Entity -> Entity -> Bool
compareEntities e1 e2 = (isEq $ typeOf (undefined :: A))
                     && (isEq $ typeOf (undefined :: B)) 
                     && (isEq $ typeOf (undefined :: C))
    where isEq :: (Eq a, Typeable a) => TypeRep a -> Bool
          isEq x = lookupComponent x e1 == lookupComponent x e2
