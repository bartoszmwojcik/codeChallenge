{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT as GLUT
import Graphics.Rendering.OpenGL.GLU.Quadrics
import Data.IORef
import System.Exit
import Control.Monad (unless)
import System.Random
import Control.Monad
import Control.Monad.State
import Data.List (foldl')


data Ball = Ball
  { ballPosition :: (GL.GLfloat, GL.GLfloat, GL.GLfloat)
  , ballRadius :: GL.GLdouble   
  , ballColor :: Color4 GLfloat
  , ballVelocity :: (GL.GLfloat, GL.GLfloat, GL.GLfloat)
  , ballShininess :: GLfloat
  } deriving (Show)


data World = World
    { balls :: [Ball]
    , side :: Float 
    , size :: Int 
    , paused :: Bool
    , debug :: Bool
    , angleX :: GL.GLfloat
    , angleY :: GL.GLfloat
    , angleZ :: GL.GLfloat
    , distance :: GL.GLfloat
    }

randomBall :: IO Ball
randomBall = do
  x <- randomRIO (-400, 400)
  y <- randomRIO (-400, 400)
  z <- randomRIO (-400, 400)
  r <- randomRIO (20, 80)
  dx <- randomRIO (-2, 2)
  dy <- randomRIO (-2, 2)
  dz <- randomRIO (-2, 2)
  red <- randomRIO (0, 1)
  green <- randomRIO (0, 1)
  blue <- randomRIO (0, 1)
  shine <- randomRIO (0,128)
  return $ Ball (x, y, z) r (Color4 red green blue 0.2) (dx, dy, dz) shine   

drawBall :: Ball -> IO ()
drawBall ball@Ball {..} = do
    let (x,y,z) = ballPosition
    translate (Vector3 x y z)
    --color ballColor
    materialAmbient Front $= ballColor
    materialDiffuse Front $= ballColor
    materialSpecular Front $= ballColor
    materialShininess Front $= ballShininess  
    renderObject Solid (Sphere' ballRadius 32 32)

pointZero :: GL.Vector3 GL.GLfloat
pointZero = GL.Vector3 0 0 0

theSize :: Float
theSize = 800

initialWorld :: Ball -> World
initialWorld ball = World
    { balls = ball : []
    , side = 800
    , size = 1
    , paused = False
    , debug = False
    , angleX = 0
    , angleY = 0
    , angleZ = 0
    , distance = -100
    }

blue,red,green,yellow,purple,white,black,lightGrey :: Color4 GLfloat
blue   = Color4 0   0   1   1
red    = Color4 1   0   0   1
green  = Color4 0   1   0   1
yellow = Color4 1   1   0   1
white  = Color4 1   1   1   1
black  = Color4 0   0   0   1
purple = Color4 0.5 0   0.5 1
lightGrey = Color4 0.8 0.8 0.8 1

main :: IO ()
main = do
    (progname, _args) <- GLUT.getArgsAndInitialize
    initialDisplayMode $= [GLUT.DoubleBuffered, GLUT.RGBMode, GLUT.WithDepthBuffer]
    GLUT.createWindow "3D Box"
    windowSize $= Size 800 800
    fstBall <- randomBall
    worldRef <- newIORef $ initialWorld fstBall
    displayCallback $= display worldRef
    idleCallback $= Just (idle worldRef)
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just (keyboardMouse worldRef)
    depthFunc $= Just Lequal
    depthMask $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    clearColor $= Color4 1 1 1 0
    let ambientMaterial = Color4 0 0 0 1
        diffuseMaterial = Color4 0 0 0 1  -- Red color
        specularMaterial = Color4 1 0 0 0  -- No specular reflection
    
    materialAmbient Front $= ambientMaterial
    materialDiffuse Front $= diffuseMaterial
    materialSpecular Front $= Color4 0 0 0 0
    materialShininess Front $= 2

    GLUT.mainLoop

display :: IORef World -> IO ()
display worldRef = do
    clearColor $= Color4 1 1 1 1
    clear [ColorBuffer, DepthBuffer]
-- Set up the view transformation
    matrixMode $= Modelview 0
    loadIdentity
    lookAt (Vertex3 0 0 1200) (Vertex3 0 0 0) (Vector3 0 1 0)

  -- Set up the light source
    let ambientLight = Color4 0.1 0.1 0.1 1
        diffuseLight = Color4 0.3 0.3 0.3 1
        specularLight = Color4 1 1 1 1
        --lightPosition = Vertex4 400 400 400 1
        lightPosition = Vertex4 100 100 1200 1
    ambient (Light 0) $= ambientLight
    diffuse (Light 0) $= diffuseLight
    specular (Light 0) $= specularLight
    lighting $= Enabled
    light (Light 0) $= Enabled
    position (Light 0) $= lightPosition

    world <- readIORef worldRef
    GL.preservingMatrix $ do
        GL.translate (GL.Vector3 0 0 (distance world))
        GL.rotate (angleX world) (GL.Vector3 1 0 (0 :: GL.GLfloat))
        GL.rotate (angleY world) (GL.Vector3 0 1 (0 :: GL.GLfloat))        
        GL.rotate (angleZ world) (GL.Vector3 0 0 (1 :: GL.GLfloat))        
        --position (Light 0) $= lightPosition
        GL.preservingMatrix $ do
            drawBoxFrame (side world) lightGrey black 
        mapM_ (\ball -> preservingMatrix $ do 
                    drawBall ball
            ) (balls world)
    swapBuffers

idle :: IORef World -> IO () 
idle worldRef = do
    world <- readIORef worldRef
    when (debug world) (mapM_ (putStrLn . show) (balls world))
    unless (paused world) $
        modifyIORef worldRef (\w -> w { balls = map (moveBall (balls w)) (balls w)
                                       , angleX = angleX w + 0.1
                                       , angleY = angleY w + 0.05
                                       , angleZ = angleZ w + 0.01
                            })    
    postRedisplay Nothing
   where 
        moveBall bs b = b { ballPosition = controlPos newP
                          , ballVelocity = (newVel bx vx, newVel by vy, newVel bz vz) 
                          }
            where (vx,vy,vz) = ballVelocity bac
                  bac = foldl' bounceBalls b bs  -- ball after collisions
                  newP@(bx,by,bz) = newPos (ballPosition bac) (ballVelocity bac)
        newPos (x,y,z) (dx,dy,dz) = (x+dx, y+dy, z+dz)
        controlPos (x,y,z) = (controlX x, controlX y, controlX z)
        newVel x dx | x < -halfSize || x > halfSize = (-1) * dx
                    | otherwise                     = dx
        halfSize = theSize / 2
        controlX x | x < -halfSize = -halfSize
                   | x > halfSize = halfSize
                   | otherwise    = x

-- | First ball after collision.        
bounceBalls :: Ball -> Ball -> Ball
bounceBalls b1 b2 | center1 == center2          = b1 -- case of the same ball controlled against itself
--                  | ballsOvelap b1 b2        -- case balls overlap, the smaller should change velocity
--                    && ballRadius b1 < ballRadius b2 
--                                        = b1 { ballVelocity = negative $ ballVelocity b2 }
                  | ballsTouch b1 b2 &&
                    distanceDecreasing b1 b2    = b1 { ballVelocity = ballVelocity b2 }
                  | otherwise                   = b1
    where                  
        ballsTouch :: Ball -> Ball -> Bool
        ballsTouch b1 b2 = distance center1 center2 <= realToFrac (ballRadius b1 + ballRadius b2)
        ballsOvelap b1 b2 = distance center1 center2 <= realToFrac (ballRadius b1)
                        ||  distance center1 center2 <= realToFrac (ballRadius b2)
        center1 = ballPosition b1
        center2 = ballPosition b2
        distance :: (GLfloat, GLfloat, GLfloat) -> (GLfloat, GLfloat, GLfloat) -> GLfloat
        distance (x1, y1, z1) (x2, y2, z2) =
            sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2 + (z2 - z1) ** 2)
        negative (x,y,z) = (-x,-y,-z)
        distanceDecreasing :: Ball -> Ball -> Bool
        distanceDecreasing b1 b2 = distance center1 center2 > distance (newC b1) (newC b2)
            where
                newC b = (x+dx,y+dy,z+dz)
                    where
                        (x,y,z) = ballPosition b
                        (dx,dy,dz) = ballVelocity b


reshape :: Size -> IO () 
reshape size@(Size w h) = do
    viewport $= (Position 0 0, size)
    matrixMode $= Projection
    loadIdentity
    perspective 45 (fromIntegral w / fromIntegral h) 1 10000
    matrixMode $= Modelview 0
    loadIdentity

keyboardMouse :: IORef World -> Key -> KeyState -> Modifiers -> Position -> IO ()
keyboardMouse worldRef (Char 'x') Down _ _ = modifyIORef worldRef (\w -> w { angleX = angleX w + 0.1 })
keyboardMouse worldRef (Char 'X') Down _ _ = modifyIORef worldRef (\w -> w { angleX = angleX w - 0.1 })
keyboardMouse worldRef (Char 'y') Down _ _ = modifyIORef worldRef (\w -> w { angleY = angleY w + 0.1 })
keyboardMouse worldRef (Char 'Y') Down _ _ = modifyIORef worldRef (\w -> w { angleY = angleY w - 0.1 })
keyboardMouse worldRef (Char 'z') Down _ _ = modifyIORef worldRef (\w -> w { angleZ = angleZ w + 0.1 })
keyboardMouse worldRef (Char 'Z') Down _ _ = modifyIORef worldRef (\w -> w { angleZ = angleZ w - 0.1 })
keyboardMouse worldRef (Char '-') Down _ _ = modifyIORef worldRef (\w -> w { distance = distance w - 100 })
keyboardMouse worldRef (Char '+') Down _ _ = modifyIORef worldRef (\w -> w { distance = distance w + 100 })
keyboardMouse worldRef (SpecialKey KeyF1) Down _ _ = randomBall >>= \nextBall -> modifyIORef worldRef (\w -> w { balls = nextBall : balls w })
keyboardMouse worldRef (SpecialKey KeyF2) Down _ _ = modifyIORef worldRef (\w -> w { debug = not $ debug w})
keyboardMouse worldRef (Char ' ') Down _ _ = modifyIORef worldRef (\w -> w { paused = not $ paused w})
keyboardMouse _ (Char 'q') Down _ _ = exitSuccess
keyboardMouse _ _ _ _ _ = return ()

drawBoxFrame :: GLfloat -> Color4 GLfloat -> Color4 GLfloat -> IO ()
drawBoxFrame size faceColor edgeColor = do
    let s2 = size / 2
    renderPrimitive LineLoop $ do
        vertex $ Vertex3 (-s2) (-s2) s2
        vertex $ Vertex3 s2 (-s2) s2
        vertex $ Vertex3 s2 s2 s2
        vertex $ Vertex3 (-s2) s2 s2
    renderPrimitive LineLoop $ do
        vertex $ Vertex3 (-s2) (-s2) (-s2)
        vertex $ Vertex3 (-s2) s2 (-s2)
        vertex $ Vertex3 s2 s2 (-s2)
        vertex $ Vertex3 s2 (-s2) (-s2)
    renderPrimitive LineLoop $ do
        vertex $ Vertex3 (-s2) (-s2) (-s2)
        vertex $ Vertex3 (-s2) (-s2) s2
        vertex $ Vertex3 (-s2) s2 s2
        vertex $ Vertex3 (-s2) s2 (-s2)
    renderPrimitive LineLoop $ do
        vertex $ Vertex3 s2 (-s2) (-s2)
        vertex $ Vertex3 s2 s2 (-s2)
        vertex $ Vertex3 s2 s2 s2
        vertex $ Vertex3 s2 (-s2) s2
    renderPrimitive LineLoop $ do
        vertex $ Vertex3 (-s2) s2 (-s2)
        vertex $ Vertex3 (-s2) s2 s2
        vertex $ Vertex3 s2 s2 s2
        vertex $ Vertex3 s2 s2 (-s2)
    renderPrimitive LineLoop $ do
        -- Bottom face
        vertex $ Vertex3 (-s2) (-s2) (-s2)
        vertex $ Vertex3 s2 (-s2) (-s2)
        vertex $ Vertex3 s2 (-s2) s2
        vertex $ Vertex3 (-s2) (-s2) s2



