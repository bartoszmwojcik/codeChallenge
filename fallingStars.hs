{-# LANGUAGE RecordWildCards #-}

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Control.Monad (forM)
import System.IO.Unsafe (unsafePerformIO)

data Element = Element
  { x :: Float
  , y :: Float
  , z :: Float
  }

type Elements = [Element]

data World = World
  { elements :: Elements
  , speed    :: Float
  }

elementRadius :: Element -> Float
elementRadius Element{..} = (401 - z) / 100 + 1

randomElement :: IO Element
randomElement = do
  x <- randomRIO (-400, 400)
  y <- randomRIO (-400, 400)
  z <- randomRIO (10, 400)
  return $ Element x y z

initialElements :: IO Elements
initialElements = forM [1..800] $ const randomElement

moveElement :: Element -> Element
moveElement Element{..}
  | z < 1 = newElement
  | otherwise = Element newX newY newZ
  where
    newX = x + x / z
    newY = y + y / z
    newZ = z - 1
    newElement = unsafePerformIO randomElement

updateElements :: Elements -> Elements
updateElements = map moveElement

updateWorld :: Float -> World -> IO World
updateWorld _ world@World{..} = return $ world
  { elements = updateElements elements }

drawElement :: Element -> Picture
drawElement Element{..} =
  translate x y $ color white $ circleSolid (elementRadius Element{..})

drawWorld :: World -> IO Picture
drawWorld World{..} = return $ pictures $ map drawElement elements

handleEvent :: Event -> World -> IO World
handleEvent (EventMotion (x, _)) world@World{..} =
  return $ world { speed = minSpeed + (x / 800) * (maxSpeed - minSpeed) }
  where
    minSpeed = 0.1
    maxSpeed = 2
handleEvent _ world = return world

main :: IO ()
main = do
  elements <- initialElements
  let world = World elements 1
  playIO (InWindow "Animation" (800, 800) (0, 0)) black 60 world drawWorld handleEvent updateWorld
