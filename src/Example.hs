{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Example where

import Data.Data
import qualified Data.Map as M
import Functions
import GHC.Generics
import Types

-- | Position component
data Position = Position
  { pos_x :: Int,
    pos_y :: Int
  }
  deriving (Show, Eq, Data, Typeable, Generic)

-- | Name component
data Name
  = Name
      { name :: String
      }
  | Name2
      { name :: String
      }
  deriving (Show, Eq, Data, Typeable, Generic)

---------- DEBUGGING FUNCTIONS ----------

printWorld :: World -> IO ()
printWorld (World e) = do
  mapM_ printEntity (M.toList e)

printEntity :: (EName, Entity) -> IO ()
printEntity (n, e) = do
  putStrLn $ "\n-----Entity " ++ show n ++ "-----"
  print e

mkEntity :: Entity
mkEntity = E $ M.fromList [(typeRep (Proxy :: Proxy Position), (C $ CD (Position 0 0))), (typeRep (Proxy :: Proxy Name), (C $ CD (Name "Entity")))]

mkEntity2 :: Entity
mkEntity2 = E $ M.fromList [(typeRep (Proxy :: Proxy Position), (C $ CD (Position 1 1))), (typeRep (Proxy :: Proxy Name), (C $ CD (Name2 "Entity2")))]

mkWorld :: World
mkWorld = World $ M.fromList [(1, mkEntity), (2, mkEntity2)]

move1 :: World -> World
move1 = mapW ((Proxy :: Proxy Position)) (\(Position x y) -> Position (x + 1) y)

printNames :: World -> IO ()
printNames w = mapWIO ((Proxy :: Proxy Name)) printName w
  where
    printName :: Name -> IO ()
    printName (Name n) = putStrLn ("1:" ++ n)
    printName (Name2 n) = putStrLn ("2:" ++ n)

goMove1 :: IO ()
goMove1 = do
  let w = runStep [move1] mkWorld
  printWorld w

goPrintNames :: IO ()
goPrintNames = do
  let w = runStep [move1] mkWorld
  printNames w
