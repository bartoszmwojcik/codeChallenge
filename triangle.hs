import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game

type Triangle = (Point, Point, Point)
type Sierpinski = [Triangle]

triangle :: Triangle
triangle = ((-1, -1), (1, -1), (0, 1))

nextGeneration :: Sierpinski -> Sierpinski
nextGeneration triangles = [subtriangle | triangle <- triangles, subtriangle <- subtriangles triangle]
  where
    subtriangles ((x1, y1), (x2, y2), (x3, y3)) =
      [ ((x1, y1), midpoint (x1, y1) (x2, y2), midpoint (x1, y1) (x3, y3))
      , (midpoint (x1, y1) (x2, y2), (x2, y2), midpoint (x2, y2) (x3, y3))
      , (midpoint (x1, y1) (x3, y3), midpoint (x2, y2) (x3, y3), (x3, y3))
      ]
    midpoint (x1, y1) (x2, y2) = ((x1 + x2) / 2, (y1 + y2) / 2)

zoomFactor :: Float
zoomFactor = 200

triangleToPicture :: Triangle -> Picture
triangleToPicture (a, b, c) = color white $ polygon [a, b, c]

iterateSierpinski :: Int -> Sierpinski -> [Sierpinski]
iterateSierpinski 0 triangles = [triangles]
iterateSierpinski n triangles = triangles : iterateSierpinski (n - 1) (nextGeneration triangles)

sierpinskiEvolution :: [Sierpinski]
sierpinskiEvolution = iterateSierpinski 5 [triangle]

data World = World { worldViewport :: ViewPort, worldTriangleIndex :: Int }

handleInput :: Event -> World -> IO World
handleInput (EventKey (SpecialKey KeySpace) Down _ _) world = return world { worldTriangleIndex = (worldTriangleIndex world + 1) `mod` length sierpinskiEvolution }
handleInput _ world = return world

update :: Float -> World -> IO World
update _ world = return world

render :: World -> IO Picture
render world = return $ applyViewPortToPicture (worldViewport world) $ pictures $ fmap triangleToPicture (sierpinskiEvolution !! worldTriangleIndex world)

initialWorld :: World
initialWorld = World { worldViewport = viewPortInit { viewPortScale = zoomFactor }, worldTriangleIndex = 0 }

main :: IO ()
main = playIO (InWindow "Sierpinski's Triangle" (800, 800) (100, 100)) black 1 initialWorld render handleInput update
