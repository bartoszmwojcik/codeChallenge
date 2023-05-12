import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game

type CarpetPoint = (Int, Int)
type Carpet = [CarpetPoint]

square :: Carpet
square = [(x, y) | x <- [0..2], y <- [0..2]]

nextGeneration :: Carpet -> Carpet
nextGeneration carpet = [(3 * x + dx, 3 * y + dy) | (x, y) <- carpet, dx <- [0..2], dy <- [0..2], not (dx == 1 && dy == 1)]

zoomFactor :: Float
zoomFactor = 8

carpetToPicture :: Carpet -> Picture
carpetToPicture carpet = pictures [translate (fromIntegral x * zoomFactor) (fromIntegral y * zoomFactor) $ color white $ rectangleSolid zoomFactor zoomFactor | (x, y) <- carpet]

iterateCarpet :: Int -> Carpet -> [Carpet]
iterateCarpet 0 carpet = [carpet]
iterateCarpet n carpet = carpet : iterateCarpet (n - 1) (nextGeneration carpet)

carpetEvolution :: [Carpet]
carpetEvolution = iterateCarpet 5 square

data World = World { worldViewport :: ViewPort, worldCarpetIndex :: Int }

handleInput :: Event -> World -> IO World
handleInput (EventKey (SpecialKey KeySpace) Down _ _) world = return world { worldCarpetIndex = (worldCarpetIndex world + 1) `mod` length carpetEvolution }
handleInput _ world = return world

update :: Float -> World -> IO World
update _ world = return world

centerCarpetTranslation :: Carpet -> (Float, Float)
centerCarpetTranslation carpet = (-(fromIntegral xmax) * zoomFactor / 2, -(fromIntegral ymax) * zoomFactor / 2)
  where
    xmax = maximum $ map fst carpet
    ymax = maximum $ map snd carpet

render :: World -> IO Picture
render world = return $ applyViewPortToPicture (worldViewport world) $ translate centerX centerY $ carpetToPicture (carpetEvolution !! worldCarpetIndex world)
  where
    (centerX, centerY) = centerCarpetTranslation (carpetEvolution !! worldCarpetIndex world)

initialWorld :: World
initialWorld = World { worldViewport = viewPortInit, worldCarpetIndex = 0 }

main :: IO ()
main = playIO (InWindow "Sierpinski's Carpet" (800, 800) (100, 100)) black 1 initialWorld render handleInput update
