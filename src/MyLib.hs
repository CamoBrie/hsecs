{-# LANGUAGE RecordWildCards #-}

module MyLib where

import Data.List

data Universe = Universe
  { entities :: [Entity]
  }

data Entity = Entity
  { position :: Maybe Position,
    velocity :: Maybe Velocity,
    texture :: Maybe Texture
  }

newtype Position = Position (Float, Float)

newtype Velocity = Velocity (Float, Float)

newtype Texture = Texture Char

(+=+) :: Position -> Velocity -> Position
Position (x, y) +=+ Velocity (dx, dy) = Position (x + dx, y + dy)

init :: Universe
init = Universe [Entity {position = Just (Position (0, 0)), velocity = Just (Velocity (1, 1)), texture = Just (Texture 'A')}, Entity {position = Just (Position (2, 1)), velocity = Just (Velocity (1, 1)), texture = Just (Texture 'B')}, Entity {position = Just (Position (10, 0)), velocity = Nothing, texture = Just (Texture 'C')}]

stepN :: Int -> Universe -> Universe
stepN 0 u = u
stepN n u = stepN (n - 1) (step u)

step :: Universe -> Universe
step (Universe es) = Universe (map stepEntity es)

output :: Universe -> IO ()
output u = putStrLn $ render u

-- renders the universe to the terminal 80x24
render :: Universe -> String
render (Universe es) = foldl mergeStrings (intercalate "\n" $ replicate 24 (replicate 80 ' ')) (map renderEntity es)

mergeStrings :: String -> String -> String
mergeStrings (a : as) (' ' : bs) = a : mergeStrings as bs
mergeStrings (' ' : as) (b : bs) = b : mergeStrings as bs
mergeStrings (a : as) (_ : bs) = a : mergeStrings as bs
mergeStrings _ _ = []

-- 80 x 24 size, creates an 80x24 size string
renderEntity :: Entity -> String
renderEntity Entity {position = Just (Position (x, y)), texture = Just (Texture t)} = intercalate "\n" $ replicate (round y) (replicate 80 ' ') ++ [replicate (round x) ' ' ++ [t] ++ replicate (80 - round x - 1) ' '] ++ replicate (24 - round y - 1) (replicate 80 ' ')
renderEntity _ = intercalate "\n" $ replicate 24 (replicate 80 ' ')

stepEntity :: Entity -> Entity
stepEntity Entity {position = Just (p), velocity = Just (v), ..} = Entity {position = Just (p +=+ v), velocity = Just v, ..}
stepEntity e = e
