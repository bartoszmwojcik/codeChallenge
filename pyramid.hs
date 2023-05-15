-- This is not working yet.
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL
import Control.Monad
import System.Exit (exitWith, ExitCode(..))
import System.IO
import Data.IORef
import Control.Concurrent (threadDelay)


type Point3D = (GLfloat, GLfloat, GLfloat)
type Tetrahedron = (Point3D, Point3D, Point3D, Point3D)
type Sierpinski = [Tetrahedron]

initialTetrahedron :: Tetrahedron
initialTetrahedron = ((0, 0, 0), (1, 0, 0), (0.5, sqrt 3 / 2, 0), (0.5, sqrt 3 / 6, sqrt 2 / sqrt 3))

nextGeneration :: Sierpinski -> Sierpinski
nextGeneration pyramids = [subpyramid | pyramid <- pyramids, subpyramid <- subpyramids pyramid]
  where
    subpyramids (a, b, c, d) =
      [ (a, midpoint a b, midpoint a c, midpoint a d)
      , (b, midpoint b a, midpoint b c, midpoint b d)
      , (c, midpoint c a, midpoint c b, midpoint c d)
      , (d, midpoint d a, midpoint d b, midpoint d c)
      ]
    midpoint (x1, y1, z1) (x2, y2, z2) = ((x1 + x2) / 2, (y1 + y2) / 2, (z1 + z2) / 2)

iterateSierpinski :: Int -> Sierpinski -> [Sierpinski]
iterateSierpinski 0 pyramids = [pyramids]
iterateSierpinski n pyramids = pyramids : iterateSierpinski (n - 1) (nextGeneration pyramids)

sierpinskiEvolution :: [Sierpinski]
sierpinskiEvolution = iterateSierpinski 4 [initialTetrahedron]

drawTetrahedron :: Tetrahedron -> IO ()
drawTetrahedron (a, b, c, d) = renderPrimitive Triangles $ do
  materialDiffuse Front $= Color4 1 0.2 0.2 1
  materialSpecular Front $= Color4 1 1 1 1
  materialShininess Front $= 25
  mapM_ vertex (map point3DToVertex3 [a, b, c, a, b, d, b, c, d, a, c, d])

point3DToVertex3 :: Point3D -> GL.Vertex3 GLfloat
point3DToVertex3 (x, y, z) = GL.Vertex3 x y z  

display :: IORef GLfloat -> Sierpinski -> IO ()
display angleRef pyramids = do
  clearColor $= Color4 0 0 0 0
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  lookAt (Vertex3 1.5 1.5 2) (Vertex3 0.5 0.5 0) (Vector3 0 1 0)

  angle <- readIORef angleRef
  rotate angle (Vector3 1 0 0)  

  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 1.5 1.5 2 1
  ambient (Light 0) $= Color4 1 0.2 0.2 1
  diffuse (Light 0) $= Color4 1 0 0 1
  specular (Light 0) $= Color4 1 0 0 1

  preservingMatrix $ do
    rotate (-30 :: GLfloat) $ Vector3 0 1 0
    scale 0.5 0.5 (0.5 :: GLfloat)
    mapM_ drawTetrahedron pyramids
  --GLFW.swapBuffers win

main :: IO ()
main = do
  _ <- GLFW.init
  mWin <- GLFW.createWindow 800 800 "Sierpinski's Pyramid" Nothing Nothing
  case mWin of
    Nothing  -> putStrLn "Failed to create GLFW window."
    Just win -> do
      angle <- newIORef 0.0
      GLFW.init win
      mainLoop win angle
      GLFW.destroyWindow win
      GLFW.terminate


--      GLFW.makeContextCurrent mWin
--      GLFW.setWindowSizeCallback win $ Just (\_win w h  -> resize (GL.Size (fromIntegral w) (fromIntegral h)))
--      GL.shadeModel $= Smooth
--      GL.clearColor $= Color4 0 0 0 1
--      GL.blend $= Enabled
--      GL.blend $= Enabled
--      GL.blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
--      GL.depthFunc $= Just Less
--      GL.normalize $= Enabled
--      GL.texture GL.Texture2D $= GL.Enabled
--      GL.depthMask $= GL.Enabled
--      angle <- newIORef 0
--      GLFW.setKeyCallback win $ Just (\_ _ _ _ _ -> exitWith ExitSuccess)
--      resize (GL.Size 800 800)

      -- Lighting setup
--      GL.lighting $= Enabled
--      GL.light (Light 0) $= Enabled
--      GL.ambient (Light 0) $= Color4 0.1 0.1 0.1 1
--      GL.diffuse (Light 0) $= Color4 1 1 1 1
--      GL.specular (Light 0) $= Color4 1 1 1 1
--      GL.position (Light 0) $= Vertex4 2 2 2 0
      
      -- Enable color material
--      GL.colorMaterial $= Just (Front, Diffuse)

--      forever $ do
--        idle angle
--        angle' <- readIORef angle
--        display (sierpinskiEvolution !! (round angle' `mod` length sierpinskiEvolution))
--        GLFW.swapBuffers win
--        GLFW.pollEvents


resize :: Size -> IO ()
resize size@(GL.Size w h) = do
  viewport $= (GL.Position 0 0, size)
  matrixMode $= Projection
  loadIdentity
  perspective 45 (fromIntegral w / fromIntegral h) 1 100
  matrixMode $= Modelview 0

mainLoop :: GLFW.Window -> IORef GLfloat -> IO ()
mainLoop win angle = do
  closeRequested <- GLFW.windowShouldClose win
  unless closeRequested $ do
    GLFW.pollEvents
    pyramids <- iterateSierpinski 4 [initialTetrahedron] --sierpinski 3
    display angle pyramids
    GLFW.swapBuffers win
    threadDelay 20000
    modifyIORef angle (+ 0.5)
    mainLoop win angle  


idle :: IORef GLfloat -> IO ()
idle angle = do
  angle $~! (+ 0.01)
  threadDelay 10000  -- Sleep for 10 milliseconds