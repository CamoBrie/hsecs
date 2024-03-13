module UX where

-- Components
data Transform = Transform
  { position :: (Float, Float),
  }
  deriving (Component)

data Velocity = Velocity
  { velocity :: (Float, Float)
  }
  deriving (Component)

data Texture = Texture
  { texture :: Char
  }
  deriving (Component)


-- initial world
initworld :: ECS World
initworld = do
  addEntity $ Transform (0, 0) $ Texture 'A' $ Velocity (1, 1)
  addEntity $ Transform (2, 1) $ Texture 'B' $ Velocity (1, 2)
  addEntity $ Transform (10, 0) $ Texture 'C'


-- systems
move :: ECS (System (Transform, Velocity))
move = do
    (Transform (x, y), Velocity (dx, dy)) <- ask
    return $ Transform (x + dx, y + dy)

render :: ECS (System (Transform, Texture))
render = do
    (Transform (x, y), Texture t) <- ask
    return $ t ++ " at " ++ show x ++ "," ++ show y

createECS :: ECS ()
createECS = do
  registerSystem move
  registerSystem render


-- main
main :: IO ()
main = do
  ecs <- createECS
  world <- initworld
  runECS ecs world
