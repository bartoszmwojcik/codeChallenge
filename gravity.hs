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
import Data.List (foldl', unfoldr)
import Data.Vector.Unboxed (Vector, fromList, (!), zipWith)
import qualified Data.Vector.Unboxed as V

type BodyPosition = Vector GLdouble
type Velocity = Vector GLdouble
type Acceleration = Vector GLdouble

data Body = Body
  { mass :: GLdouble
  , radius :: GLdouble
  , pos :: BodyPosition
  , velocity :: Velocity
  , color :: Color4 GLfloat
  , shininess :: GLfloat
  } deriving (Show, Eq)


-- | Calculate the gravitational force between two points in 3D space.
gravitationalForce :: GLdouble -> GLdouble -> BodyPosition -> BodyPosition -> BodyPosition
gravitationalForce mass1 mass2 pos1 pos2 =
  let
    gConst = 6.67430e-11
    delta = V.zipWith (-) pos2 pos1
    distance = sqrt $ V.sum $ V.map (^2) delta
    forceMagnitude = gConst * mass1 * mass2 / (distance * distance)
  in V.map (*(forceMagnitude / distance)) delta

-- | Update the position and velocity of a body using the Euler method.
eulerStep :: GLdouble -> Body -> Body -> Body
eulerStep dt body1 body2 =
  let
    force = gravitationalForce (mass body1) (mass body2) (pos body1) (pos body2)
    acceleration = V.map (/(mass body1)) force
    newPosition = V.zipWith (+) (pos body1) $ V.map (*(dt)) (velocity body1)
    newVelocity = V.zipWith (+) (velocity body1) $ V.map (*(dt)) acceleration
  in
    body1 { pos = newPosition, velocity = newVelocity }

-- | Update the positions and velocities of all bodies in the system.
updateSystem :: GLdouble -> [Body] -> [Body]
--updateSystem dt bodies = [ foldl (eulerStep dt) body (filter (/= body) bodies) | body <- bodies]
updateSystem dt bodies = [ eulerCalc dt (acceleration bodies body) body | body <- bodies]

deltaTime :: GLdouble
deltaTime = 60 * 60 -- 1h in seconds

-- Calculate the gravitational acceleration on a body due to all other bodies
acceleration :: [Body] -> Body -> Acceleration
acceleration bodies body = foldl' (\ a b -> V.zipWith (+) a $ gravitationalAcceleration body b) (fromList [0,0,0]) bodies

eulerCalc :: GLdouble -> Acceleration -> Body -> Body
eulerCalc dt a body = 
    let
        newPosition = V.zipWith (+) (pos body) $ V.map (*(dt)) (velocity body)
        newVelocity = V.zipWith (+) (velocity body) $ V.map (*(dt)) a
    in
        body { pos = newPosition, velocity = newVelocity }

-- Calculate the gravitational acceleration on a body due to one other body
gravitationalAcceleration :: Body -> Body -> Acceleration
gravitationalAcceleration b1 b2
    | b1 == b2      = fromList [0,0,0]              -- A body doesn't gravitate towards itself
    | otherwise     = V.map (* (g * mass2 / distance^2)) unitSeparationVector
    where
        g = 6.67430e-11 -- Gravitational constant in m^3 kg^-1 s^-2
        mass2 = mass b2
        delta = V.zipWith (-) (pos b2) (pos b1)
        distance = sqrt $ V.sum $ V.map (^2) delta
        unitSeparationVector = V.map (/ distance) delta 


earth :: Body
earth = Body
  { mass = 5.972e24  -- kg
  , radius = 6371000 -- m
  , pos = fromList [0, 0, 0]
  , velocity = fromList [0,-100, 0]
  , color = lightBlue
  , shininess = 32
  }

moon :: Body
moon = Body
  { mass = 7.342e22  -- kg
  , radius = 1737000 -- m
  , pos = fromList [384400000, 0, 0]  -- m
  , velocity = fromList [0, 0, 1002]  -- m/s
  , color = lightGrey
  , shininess = 32
  }

slowMoon :: Body
slowMoon = moon { velocity = V.map (*0.7) $ velocity moon }

heavyMoon :: Body
heavyMoon = moon { mass = 10 * mass moon }

moonLike2Earths :: Body
moonLike2Earths = moon { mass = 2 * mass earth
                        , velocity = V.map (*0.5) $ velocity moon }
earthLikeHalfMoon :: Body
earthLikeHalfMoon = earth { velocity = V.map (*(-1)) $ velocity moon }

sun :: Body
sun = Body
  { mass = 2e30 -- kg
  , radius = 692342000 -- m
  , pos = fromList [0, 0, 0]
  , velocity = fromList [0,0, 0]
  , color = sunYellow
  , shininess = 256
  }

mercury :: Body
mercury = Body
  { mass = 0.33e24  -- kg
  , radius = 2440000 -- m
  , pos = fromList [57909170000, 0, 0]
  , velocity = fromList [0,48000, 0]
  , color = mercuryBrown
  , shininess = 16  
  }

venus :: Body
venus = Body
  { mass = 4.868e24  -- kg
  , radius = 6100000 -- m
  , pos = fromList [108209000000, 0, 0]
  , velocity = fromList [0,35000, 0]
  , color = venusRed
  , shininess = 16  
  }

jupiter :: Body
jupiter = Body
  { mass = 1.899e27  -- kg
  , radius = 71500000 -- m
  , pos = fromList [778412027000, 0, 0]
  , velocity = fromList [0,13000, 0]
  , color = jupiterOrange
  , shininess = 16  
  }


positionToSun pos = V.zipWith (+) pos (fromList [149597887000, 0, 0])
velocityToSun v = V.zipWith (+) v (fromList [0,30000,0])

earthWithSun = earth { pos = positionToSun $ pos earth 
                    , velocity = velocityToSun $ velocity earth }
moonWithSun = moon { pos = positionToSun $ pos moon 
                    , velocity = velocityToSun $ velocity moon
                     }

oneUnit :: GLfloat
oneUnit = 384400000

data Simulation = 
      EarthMoon         
    | EarthMoonSlower   
    | EarthMoonHeavier  
    | SunEarthMoon      
    | SmallHeavierBigLigher

viewRatio :: Simulation -> GLdouble
viewRatio SunEarthMoon  = 50000000
viewRatio _             = 1000000

viewDistance :: Simulation -> GLdouble
viewDistance SunEarthMoon  = 7000
viewDistance _args          = 2000

radiusRatio :: Simulation -> GLdouble
radiusRatio SunEarthMoon = 700000
radiusRatio _           = 50000

data World = World
    { bodies :: [Body]
    , paused :: Bool
    , debug :: Bool
    , angleX :: GL.GLfloat
    , angleY :: GL.GLfloat
    , angleZ :: GL.GLfloat
    , distance :: GL.GLfloat
    , simulation :: Simulation
    }

toVector3 :: (V.Unbox a, Real a, Fractional b) => Vector a -> Vector3 b
toVector3 vec
  | V.length vec == 3 =
      Vector3 (realToFrac (vec V.! 0) )
              (realToFrac (vec V.! 1) )
              (realToFrac (vec V.! 2) )
  | otherwise = error "Data.Vector must have exactly 3 elements"


drawBody :: GLdouble -> GLdouble -> Body -> IO ()
drawBody ratioD ratioR Body {..} = do
    --let (x,y,z) = position
    --translate (Vector3 x y z)
    translate $ (toVector3 $ V.map (\d -> d / ratioD)  pos  :: Vector3 GLdouble )
    --color ballColor
    materialAmbient Front $= color
    materialDiffuse Front $= color
    materialSpecular Front $= color
    materialShininess Front $= shininess  
    renderObject Solid (Sphere' (radius / ratioR) 32 32)

pointZero :: GL.Vector3 GL.GLfloat
pointZero = GL.Vector3 0 0 0

initialWorld :: World
initialWorld = World
    { bodies = [earth, moon]
    , paused = False
    , debug = False
    , angleX = 0
    , angleY = 0
    , angleZ = 0
    , distance = -100
    , simulation = EarthMoon
    }

blue,red,green,yellow,purple,white,black,lightGrey :: Color4 GLfloat
blue   = Color4 0   0   1   1
lightBlue = Color4 0.4 0.7 1 1
red    = Color4 1   0   0   1
green  = Color4 0   1   0   1
yellow = Color4 1   1   0   1
white  = Color4 1   1   1   1
black  = Color4 0   0   0   1
purple = Color4 0.5 0   0.5 1
lightGrey = Color4 0.8 0.8 0.8 1
sunYellow = Color4 1 1 0.6 1
venusRed = Color4 1 0.4 0.4 1
jupiterOrange = Color4 1 0.7 0.4 1
mercuryBrown = Color4  0.3 0.1 0 1

main :: IO ()
main = do
    (progname, _args) <- GLUT.getArgsAndInitialize
    initialDisplayMode $= [GLUT.DoubleBuffered, GLUT.RGBMode, GLUT.WithDepthBuffer]
    GLUT.createWindow "3D Box"
    windowSize $= Size 1200 1000
    worldRef <- newIORef initialWorld 
    displayCallback $= display worldRef
    idleCallback $= Just (idle worldRef)
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just (keyboardMouse worldRef)
    depthFunc $= Just Lequal
    depthMask $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    clearColor $= Color4 0 0 0 0
    GLUT.mainLoop

display :: IORef World -> IO ()
display worldRef = do
    clearColor $= Color4 1 1 1 1
    clear [ColorBuffer, DepthBuffer]
-- Set up the view transformation
    matrixMode $= Modelview 0
    loadIdentity
    world <- readIORef worldRef
    lookAt (Vertex3 0 0 (viewDistance $ simulation world)) (Vertex3 0 0 0) (Vector3 0 1 0)

  -- Set up the light source
    let ambientLight = Color4 0.1 0.1 0.1 1
        diffuseLight = Color4 0.3 0.3 0.3 1
        specularLight = Color4 1 1 1 1
        --lightPosition = Vertex4 400 400 400 1
        lightPosition = Vertex4 100 100 oneUnit 1
    ambient (Light 0) $= ambientLight
    diffuse (Light 0) $= diffuseLight
    specular (Light 0) $= specularLight
    lighting $= Enabled
    light (Light 0) $= Enabled
    position (Light 0) $= lightPosition

    GL.preservingMatrix $ do
        GL.translate (GL.Vector3 0 0 (distance world))
        GL.rotate (angleX world) (GL.Vector3 1 0 (0 :: GL.GLfloat))
        GL.rotate (angleY world) (GL.Vector3 0 1 (0 :: GL.GLfloat))        
        GL.rotate (angleZ world) (GL.Vector3 0 0 (1 :: GL.GLfloat))        
        --position (Light 0) $= lightPosition
        mapM_ (\body -> preservingMatrix $ do 
                    drawBody (viewRatio $ simulation world) (radiusRatio $ simulation world) body
            ) (bodies world)
    swapBuffers

idle :: IORef World -> IO () 
idle worldRef = do
    world <- readIORef worldRef
    when (debug world) (putStrLn $ show $ (pos $ head $ bodies world, velocity $ head $ bodies world))
    unless (paused world) $
        modifyIORef worldRef (\w -> w { bodies = updateSystem deltaTime $ bodies w })
{-            balls = map (moveBall (balls w)) (balls w)
                                       , angleX = angleX w + 0.1
                                       , angleY = angleY w + 0.05
                                       , angleZ = angleZ w + 0.01
                            })    
-}                                                        
    postRedisplay Nothing

reshape :: Size -> IO () 
reshape size@(Size w h) = do
    viewport $= (Position 0 0, size)
    matrixMode $= Projection
    loadIdentity
    perspective 45 (fromIntegral w / fromIntegral h) 1 10000
    matrixMode $= Modelview 0
    loadIdentity


keyboardMouse :: IORef World -> Key -> KeyState -> Modifiers -> Position -> IO ()
keyboardMouse worldRef (Char 'x') Down _ _ = modifyIORef worldRef (\w -> w { angleX = angleX w + 0.5 })
keyboardMouse worldRef (Char 'X') Down _ _ = modifyIORef worldRef (\w -> w { angleX = angleX w - 0.5 })
keyboardMouse worldRef (Char 'y') Down _ _ = modifyIORef worldRef (\w -> w { angleY = angleY w + 0.5 })
keyboardMouse worldRef (Char 'Y') Down _ _ = modifyIORef worldRef (\w -> w { angleY = angleY w - 0.5 })
keyboardMouse worldRef (Char 'z') Down _ _ = modifyIORef worldRef (\w -> w { angleZ = angleZ w + 0.5 })
keyboardMouse worldRef (Char 'Z') Down _ _ = modifyIORef worldRef (\w -> w { angleZ = angleZ w - 0.5 })
keyboardMouse worldRef (Char '-') Down _ _ = modifyIORef worldRef (\w -> w { distance = distance w - 100 })
keyboardMouse worldRef (Char '+') Down _ _ = modifyIORef worldRef (\w -> w { distance = distance w + 100 })
keyboardMouse worldRef (SpecialKey KeyF1) Down _ _ = modifyIORef worldRef (\w -> w { bodies = [earth, moon]
                                                                                    , simulation = EarthMoon })
keyboardMouse worldRef (SpecialKey KeyF2) Down _ _ = modifyIORef worldRef (\w -> w { bodies = [earth, slowMoon]
                                                                                    , simulation = EarthMoonSlower })
keyboardMouse worldRef (SpecialKey KeyF3) Down _ _ = modifyIORef worldRef (\w -> w { bodies = [earth, heavyMoon]
                                                                                    , simulation = EarthMoonHeavier })
keyboardMouse worldRef (SpecialKey KeyF4) Down _ _ = modifyIORef worldRef (\w -> w { bodies = [earthLikeHalfMoon, moonLike2Earths]
                                                                                    , simulation = SmallHeavierBigLigher })
keyboardMouse worldRef (SpecialKey KeyF5) Down _ _ = modifyIORef worldRef (\w -> w { bodies = [sun, mercury, venus, earthWithSun, moonWithSun, jupiter]
                                                                                    , simulation = SunEarthMoon })
keyboardMouse worldRef (SpecialKey KeyF12) Down _ _ = modifyIORef worldRef (\w -> w { debug = not $ debug w})
keyboardMouse worldRef (Char ' ') Down _ _ = modifyIORef worldRef (\w -> w { paused = not $ paused w})
keyboardMouse _ (Char 'q') Down _ _ = exitSuccess
keyboardMouse _ _ _ _ _ = return ()


icosahedron :: [(GL.Vertex3 GL.GLfloat, GL.TexCoord2 GL.GLfloat)]
icosahedron = concatMap (map (\v -> (v, toTexCoord v))) faces
  where
    faces = subdivide 2 $ map (map toV3) icoFaces
--    faces = subdivide 2 $ icoFaces
    toTexCoord (GL.Vertex3 x y z) = GL.TexCoord2 ((atan2 y x / (2*pi)) + 0.5) (acos z / pi)
    toV3 :: [GLfloat] -> Vertex3 GLfloat
    toV3 [x, y, z] = GL.Vertex3 x y z

icoFaces :: [[[GLfloat]]]
--icoFaces :: [Vertex3 GLfloat]
icoFaces = unfoldr f icoVerts
  where f [] = Nothing
--        f (a:b:c:xs) = Just (Vertex3 a b c, xs)
        f (a:b:c:xs) = Just ([a,b,c], xs)

icoVerts :: [[GLfloat]]
icoVerts = [
    [-x, 0.0, z],
    [x, 0.0, z],
    [-x, 0.0, -z],
    [x, 0.0, -z],
    [0.0, z, x],
    [0.0, z, -x],
    [0.0, -z, x],
    [0.0, -z, -x],
    [z, x, 0.0],
    [-z, x, 0.0],
    [z, -x, 0.0],
    [-z, -x, 0.0]
  ]
  where x = 0.525731112119133606
        z = 0.850650808352039932

--subdivide :: Int -> [[GL.Vertex3 GL.GLfloat]] -> [[GL.Vertex3 GL.GLfloat]]
subdivide 0 faces = faces
subdivide n faces = subdivide (n-1) $ concatMap subdivideFace faces
  where subdivideFace [a, b, c] = [[a, ab, ac], [ab, b, bc], [ac, bc, c], [ab, bc, ac]]
          where ab = normalizeV3 $ midpoint a b
                ac = normalizeV3 $ midpoint a c
                bc = normalizeV3 $ midpoint b c

normalizeV3 :: GL.Vertex3 GL.GLfloat -> GL.Vertex3 GL.GLfloat
normalizeV3 v@(GL.Vertex3 x y z) = GL.Vertex3 (x/l) (y/l) (z/l)
  where l = sqrt (x*x + y*y + z*z)

midpoint :: GL.Vertex3 GL.GLfloat -> GL.Vertex3 GL.GLfloat -> GL.Vertex3 GL.GLfloat
midpoint (GL.Vertex3 x1 y1 z1) (GL.Vertex3 x2 y2 z2) = GL.Vertex3 ((x1+x2)/2) ((y1+y2)/2) ((z1+z2)/2)

