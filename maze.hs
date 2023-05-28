import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate
import System.Random
import Control.Monad.State
import Data.List (delete)
import qualified Data.Map as Map

data Direction = North | East | South | West deriving (Eq, Enum, Bounded, Show)
instance Random Direction where
  random g = case randomR (0, 3) g of
    (r, g') -> (toEnum r, g')
  randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
    (r, g') -> (toEnum r, g')

data Cell = Cell Int Int deriving (Eq, Ord, Show)
type Maze = Map.Map Cell [Direction]
type Model = (Maze, [Cell], StdGen)

mazeSize :: Int
mazeSize = 40

cellSize :: Float
cellSize = 20

opposite :: Direction -> Direction
opposite North = South
opposite East = West
opposite South = North
opposite West = East

adjacent :: Cell -> Direction -> Cell
adjacent (Cell x y) dir = case dir of
  North -> Cell x (y + 1)
  East -> Cell (x + 1) y
  South -> Cell x (y - 1)
  West -> Cell (x - 1) y

valid :: Cell -> Bool
valid (Cell x y) = x >= 0 && y >= 0 && x < mazeSize && y < mazeSize

notVisited :: Maze -> Cell -> Bool
notVisited maze cell = case Map.lookup cell maze of
  Nothing   -> False
  Just dirs -> length dirs == 4

modelIteration :: ViewPort -> Float -> Model -> Model
modelIteration vp t (maze,cells, gen) = 
  case cells of
    [] -> (maze,[], gen)
    c:cs -> do
      let (dirs, gen') = shuffle [North, East, South, West] gen
      let validDirs = filter (valid . adjacent c) dirs
      case [d | d <- validDirs, notVisited maze (adjacent c d)] of
        [] -> (maze, cs, gen')  -- no unvisited neighbors, backtrack
        d: _ -> do  -- choose one unvisited neighbor
          let newCell = adjacent c d
          ((Map.adjust (delete d) c $ Map.adjust (delete (opposite d)) newCell maze), (newCell:c:cs), gen')


createEmptyMaze :: Maze
createEmptyMaze = Map.fromList [(Cell x y, [North, East, South, West]) | x <- [0..mazeSize - 1], y <- [0..mazeSize - 1]]

initialModel :: StdGen -> Model
initialModel g = (createEmptyMaze, [Cell 0 0], g)

--drawMaze :: Maze -> Picture
drawMaze :: Model -> Picture
drawMaze (maze, cells, _) = pictures $ map drawCell (Map.toList maze) ++ map drawRoute cells
  where
    drawRoute (Cell x y) = trans $ color (greyN 0.5) $ thickCircle (cellSize / 8) (cellSize / 4)
      where
        halfMazeSize = fromIntegral mazeSize * cellSize / 2
        trans = translate (fromIntegral x * cellSize - halfMazeSize) (fromIntegral y * cellSize - halfMazeSize)
    drawCell (Cell x y, dirs) = pictures [cellPic, northWall, eastWall, southWall, westWall]
      where
        halfMazeSize = fromIntegral mazeSize * cellSize / 2
        trans = translate (fromIntegral x * cellSize - halfMazeSize) (fromIntegral y * cellSize - halfMazeSize)
        cellColor = if length dirs == 4 then greyN 0.5 else white
        cellPic = trans $ color cellColor $ rectangleSolid cellSize cellSize
        c2 = cellSize / 2
        northWall = if North `elem` dirs then wallPic (-c2, c2) (c2, c2) else blank
        eastWall = if East `elem` dirs then wallPic (c2, -c2) (c2, c2) else blank
        southWall = if South `elem` dirs then wallPic (-c2, -c2) (c2, -c2) else blank
        westWall = if West `elem` dirs then wallPic (-c2, -c2) (-c2, c2) else blank
        wallPic x y = trans $ color black $ line [x, y]


-- Randomly shuffle a list
shuffleM :: [a] -> State StdGen [a]
shuffleM xs = if length xs < 2 then return xs else do
    i <- state $ randomR (0, length xs - 1)
    r <- shuffleM (take i xs ++ drop (i+1) xs)
    return (xs !! i : r) 

shuffle :: [a] -> StdGen -> ([a], StdGen)
shuffle xs g = if length xs < 2 then (xs, g) else do
    let (i,g') = randomR (0, length xs - 1) g
    let (r,g'') = shuffle (take i xs ++ drop (i+1) xs) g'
    (xs !! i : r, g'') 


main :: IO ()
main = do
  gen <- newStdGen
  simulate
    (InWindow "Maze" (mazeSize * round (cellSize * 1.5), mazeSize * round (cellSize * 1.5)) (0, 0))
    (greyN 0.5)
    32
    (initialModel gen)
    drawMaze
    modelIteration