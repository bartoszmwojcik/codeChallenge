import System.Random (randomR, StdGen, newStdGen)
import Control.Monad (liftM2)

-- Additional imports
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

-- Add a field to store the random generator in the World data type
data World = World
  { snake :: [(Float, Float)]
  , direction :: (Float, Float)
  , food :: (Float, Float)
  , gen :: StdGen
  , score :: Int
  }

initialWorld :: StdGen -> World
initialWorld g = World
  { snake = [(0, 0)]
  , direction = (20, 0)
  , food = (60, 60)
  , gen = g
  , score = 0
  }

-- Modify the main function to initialize the random generator
main :: IO ()
main = do
  g <- newStdGen
  play
    (InWindow "Snake" (800, 800) (10, 10))
    white
    4
    (initialWorld g)
    renderWorld
    handleInput
    updateWorld

renderWorld :: World -> Picture
renderWorld world = pictures
  [ drawSnake (snake world)
  , drawFood (food world)
  , drawScore (score world)
  ]

drawScore :: Int -> Picture
drawScore score = translate (-350) 350 $ scale 0.2 0.2 $ color black $ text $ "Score: " ++ show score

-- Complete the drawSnake function
drawSnake :: [(Float, Float)] -> Picture
drawSnake segments = pictures $ map drawSegment segments
  where
    drawSegment (x, y) = translate x y $ color green $ rectangleSolid 20 20

-- Complete the drawFood function
drawFood :: (Float, Float) -> Picture
drawFood (x, y) = translate x y $ color red $ rectangleSolid 20 20

-- Complete the handleInput function
handleInput :: Event -> World -> World
handleInput (EventKey (SpecialKey KeyUp) _ _ _) world = world { direction = (0, 20) }
handleInput (EventKey (SpecialKey KeyDown) _ _ _) world = world { direction = (0, -20) }
handleInput (EventKey (SpecialKey KeyLeft) _ _ _) world = world { direction = (-20, 0) }
handleInput (EventKey (SpecialKey KeyRight) _ _ _) world = world { direction = (20, 0) }
handleInput _ world = world

-- Complete the updateWorld function
updateWorld :: Float -> World -> World
updateWorld _ world =
  let newHead = addTuples (head $ snake world) (direction world)
      hasEatenFood = newHead == food world
      newTail = if hasEatenFood then snake world else init (snake world)
      newSnake = newHead : newTail
      (newFood, newGen) = if hasEatenFood
                          then generateFood (gen world)
                          else (food world, gen world)
      newScore = if hasEatenFood then score world + 1 else score world
      gameOver = hasLost newHead newSnake
  in if gameOver
        then initialWorld newGen
        else world { snake = newSnake, food = newFood, gen = newGen, score = newScore }  
--  in world { snake = newSnake, food = newFood, gen = newGen }


-- Helper function to add two 2D points (tuples)
addTuples :: (Float, Float) -> (Float, Float) -> (Float, Float)
addTuples (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Function to generate a new food position
generateFood :: StdGen -> ((Float, Float), StdGen)
generateFood g =
  let (x, g') = randomR (-19, 19) g :: (Int, StdGen)
      (y, g'') = randomR (-19, 19) g' :: (Int, StdGen)
      alignedX = fromIntegral x * 20
      alignedY = fromIntegral y * 20
  in ((alignedX, alignedY), g'')

hasLost :: (Float, Float) -> [(Float, Float)] -> Bool
hasLost headPos snakeSegments =
  let (x, y) = headPos
      outOfBounds = x > 380 || x < -380 || y > 380 || y < -380
      collisionWithSelf = any (\p -> p == headPos) (tail snakeSegments)
  in outOfBounds || collisionWithSelf
