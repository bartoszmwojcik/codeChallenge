{-# LANGUAGE RecordWildCards #-}
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Geometry.Angle as Angle
import Graphics.Gloss.Interface.IO.Game (Event(..), Key(..), KeyState(..), playIO, SpecialKey (KeySpace, KeyF1, KeyF2, KeyF3, KeyF4, KeyF5))

data World = World
  { time :: Float,
    paused :: Bool,
    xHistory :: Float,
    yHistory :: [Float],
    radius :: Float,
    frequency :: Float,
    pics :: Picture,
    wave :: Wave
  }

data Wave = Square
          | SawTooth
          | Geometric
          | Linear
          | Triangle

-- | Function building fouier series for give wave
fourierSeries :: Wave -> Int -> (Int, Int)
fourierSeries Square i = (2*i-1, 2*i-1)
fourierSeries SawTooth i = (i, -i)
fourierSeries Geometric i = (i*i,i*i)
fourierSeries Linear i = (i,i)
fourierSeries Triangle i = (j, (-1)^(i-1) * j * j)
    where j = fromIntegral $ 2*i-1


fps :: Int
fps = 180

circleSeriesLength :: Int
circleSeriesLength = 9

circleRadius :: Float
circleRadius = 200

-- Main function
main :: IO ()
main = playIO (InWindow "Circle Animation" (1200, 600) (10, 10)) white fps (World 0 False 0 [0] circleRadius 0.5 Blank Square) drawWorld handleEvent updateWorld

drawWorld :: World -> IO Picture
drawWorld world = return $ drawCircleAnim world

handleEvent :: Event -> World -> IO World
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) world = return $ world { paused = not (paused world) }
handleEvent (EventKey (SpecialKey KeyF1) Down _ _) _ = return $ World 0 False 0 [0] circleRadius 0.5 Blank Square
handleEvent (EventKey (SpecialKey KeyF2) Down _ _) _ = return $ World 0 False 0 [0] circleRadius 0.5 Blank SawTooth
handleEvent (EventKey (SpecialKey KeyF3) Down _ _) _ = return $ World 0 False 0 [0] circleRadius 0.5 Blank Geometric
handleEvent (EventKey (SpecialKey KeyF4) Down _ _) _ = return $ World 0 False 0 [0] circleRadius 0.5 Blank Linear
handleEvent (EventKey (SpecialKey KeyF5) Down _ _) _ = return $ World 0 False 0 [0] circleRadius 0.5 Blank Triangle
handleEvent _ world = return world

updateWorld :: Float -> World -> IO World
updateWorld deltaTime world
  | paused world = return world
  | otherwise = return $ world { time = time world + deltaTime, xHistory = x, yHistory = newYHistory, pics = circleComponents }
  where
    newYHistory = y : yHistory world
    (pics, x, y) = foldl oneCircleComponents ([],0,0) [1 .. circleSeriesLength]
    circleComponents = pictures pics

    oneCircleComponents :: ([Picture], Float, Float) -> Int -> ([Picture], Float, Float)
    oneCircleComponents (pics, dx, dy) i = 
                        (map (translate dx dy) [oneCircleOutline, oneConnectingLineToCenter] ++ pics
                        , x + dx
                        , y + dy)
        where
            (k,l) = fourierSeries (wave world) i
            oneCircleOutline = color black $ circle (radius world / fromIntegral  l)
            oneConnectingLineToCenter = color blue $ line [(0, 0), (x, y)]
            angle = 2 * pi * frequency world * time world * fromIntegral  k
            x = radius world / fromIntegral l * cos angle
            y = radius world / fromIntegral l * sin angle

-- Draw the circle, moving point, and additional animation
drawCircleAnim :: World -> Picture
drawCircleAnim World {..} = translate (-350) 0 $ pictures [pics, pointComponents, traceComponents]
  where
    pointComponents = pictures [movingPoint', connectingLineBetweenPoints]
    traceComponents = pictures $ zipWith3 tracePoint [0..] (take 600 yHistory) (drop 1 $ take 601 yHistory)   

    movingPoint' = translate xOffset yOffset $ color green $ circleSolid 5
    xOffset = 2 * radius
    yOffset = head yHistory
    connectingLineBetweenPoints = color orange $ line [(xHistory, yOffset), (xOffset, yOffset)]

    tracePoint :: Int -> Float -> Float -> Picture
    tracePoint idx yValue1 yValue2 = color green $ line [(xOffset + fromIntegral idx, yValue1), (xOffset + fromIntegral idx + 1, yValue2)]

