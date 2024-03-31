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

e1 :: Entity
e1 = mkEntity >:> (Position 0 0) >:> (Name "Entity")

e2 :: Entity
e2 = mkEntity >:> (Position 1 1) >:> (Name2 "Entity2")

initialWorld :: World
initialWorld = mkWorld [e1, e2]

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
  let w = runStep [move1] initialWorld
  printWorld w

goPrintNames :: IO ()
goPrintNames = do
  let w = runStep [move1] initialWorld
  printNames w
