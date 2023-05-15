-- | Simulation of 3D gravitational forces implemented using euler method.
--   Five examples implemented using OpenGL for 3D animations. All examples take real world values 
--   and apply to them gravitional forces in order to calculate new positions. Euler method is known as
--   having accrual numerical errors, so the solution is not very precise.
--   F1. Earth and Moon
--   F2. Earth and Moon slower than in reality
--   F3. Earth and Moon 10x heavier
--   F4. Earth and Moon 2x heavier than the Earth.
--   F5. Sun, Mercury, Venus, Earth and Moon.
--   Manipulation of the point of view (aka camera, eye): x,X,y,Y,z,Z.
--   Manipulation of the point of camera focus:
--   0 - in the point 0,0,0
--   1 - on the 1st celestial body of the example
--   2..n - on the 2nd,.., n-th celestial body of the example
--   SPACE - pause
--   q - exit

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
import Data.Vector.Unboxed (Vector (..), fromList, (!), zipWith)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Storable as VS
import Codec.Picture as JP

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
  , texture :: Maybe TextureObject
  , angle :: Vector GLfloat
  , aVel :: Vector GLfloat
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
        newAngle = V.zipWith (+) (angle body) (aVel body)
    in
        body { pos = newPosition, velocity = newVelocity, angle = newAngle}

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


earth :: IO Body
earth = loadTexture "./images/earth_daymap.jpg" >>= \t -> return Body
  { mass = 5.972e24  -- kg
  , radius = 6371000 -- m
  , pos = fromList [0, 0, 0]
  , velocity = fromList [0,-10, 0]
  , color = lightBlue
  , shininess = 32
  , angle = fromList [(-90), 0, 0]
  , aVel = fromList  [0, 0, 3]
  , texture = Just t
  }

moon :: IO Body
moon = loadTexture "./images/moon.jpg" >>= \t -> return Body
  { mass = 7.342e22  -- kg
  , radius = 1737000 -- m
  , pos = fromList [384400000, 0, 0]  -- m
  , velocity = fromList [0, 0, 1002]  -- m/s
  , color = lightGrey
  , shininess = 32
  , texture = Just t
  , angle = fromList [0, 0, 0]
  , aVel = fromList [0,-0.3, 0]
  }

slowMoon :: IO Body
slowMoon = moon >>= \m ->
           return m { velocity = V.map (*0.7) $ velocity m }

heavyMoon :: IO Body
heavyMoon = moon >>= \m ->
            return m { mass = 10 * mass m }

moonLike2Earths :: IO Body
moonLike2Earths = earth >>= \e -> 
                    moon >>= \m ->
                    return m { mass = 2 * mass e
                            , velocity = V.map (*0.5) $ velocity m }
earthLikeHalfMoon :: IO Body
earthLikeHalfMoon = earth >>= \e -> 
                    moon >>= \m ->
                    return e { velocity = V.map (*(-1)) $ velocity m }

sun :: IO Body
sun = loadTexture "./images/sun.jpg" >>= \t -> return Body
  { mass = 2e30 -- kg
  , radius = 692342000 -- m
  , pos = fromList [0, 0, 0]
  , velocity = fromList [0,0, 0]
  , color = sunYellow
  , shininess = 256
  , texture = Just t
  , angle = fromList [0, 0, 0]
  , aVel = fromList [0, 0.1, 0]
  }
  

mercury :: Body
mercury = Body
  { mass = 0.33e24  -- kg
  , radius = 2440000 -- m
  , pos = fromList [57909170000, 0, 0]
  , velocity = fromList [0,48000, 0]
  , color = mercuryBrown
  , shininess = 16  
  , texture = Nothing
  , angle = fromList [0, 0, 0]
  , aVel = fromList [0, 0, 0]
 }

venus :: Body
venus = Body
  { mass = 4.868e24  -- kg
  , radius = 6100000 -- m
  , pos = fromList [108209000000, 0, 0]
  , velocity = fromList [0,35000, 0]
  , color = venusRed
  , shininess = 16  
  , texture = Nothing
  , angle = fromList [0, 0, 0]
  , aVel = fromList [0, 0, 0]
}

jupiter :: Body
jupiter = Body
  { mass = 1.899e27  -- kg
  , radius = 71500000 -- m
  , pos = fromList [778412027000, 0, 0]
  , velocity = fromList [0,13000, 0]
  , color = jupiterOrange
  , shininess = 16  
  , texture = Nothing
  , angle = fromList [0, 0, 0]
  , aVel = fromList [0, 0, 0]
  }

-- In examples 1..4 the Earth is located at  0,0,0
-- In example 5, the Earth has to be moved to its position realitve to sun
-- and its velocity also has to be adapted accordingly.
-- The same for the Moon.
positionToSun pos = V.zipWith (+) pos (fromList [149597887000, 0, 0])
velocityToSun v = V.zipWith (+) v (fromList [0,30000,0])

earthWithSun = earth >>= \e -> 
                return e { pos = positionToSun $ pos e
                         , velocity = velocityToSun $ velocity e }
moonWithSun = moon >>= \m ->
                return m { pos = positionToSun $ pos m
                        , velocity = velocityToSun $ velocity m
                        }

oneUnit :: GLfloat
oneUnit = 384400000

-- The Sun is so big and so far away, that in order to keep everything visible 
-- certain proportions have to be introduced, dependend on the example
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
    , distance :: GL.GLdouble
    , simulation :: Simulation
    , center :: Maybe Int
    , eyeX :: GLdouble
    , eyeY :: GLdouble
    , eyeZ :: GLdouble
    }

toVector3 :: (V.Unbox a, Real a, Fractional b) => Vector a -> Vector3 b
toVector3 vec
  | V.length vec == 3 =
      Vector3 (realToFrac (vec V.! 0) )
              (realToFrac (vec V.! 1) )
              (realToFrac (vec V.! 2) )
  | otherwise = error "Data.Vector must have exactly 3 elements"


-- | Renders given body with gived distance and radius ratios.
drawBody :: GLdouble -> GLdouble -> Body -> IO ()
drawBody ratioD ratioR Body {..} = do
    translate $ (toVector3 $ V.map (\d -> d / ratioD) pos  :: Vector3 GLdouble )
    materialAmbient Front $= color
    materialDiffuse Front $= color
    materialSpecular Front $= color
    materialShininess Front $= shininess     
    let r = radius / ratioR
    case texture of
        Nothing -> renderObject Solid (Sphere' r 32 32)
        Just tx -> do 
            GL.texture GL.Texture2D $= GL.Enabled
            GL.textureBinding GL.Texture2D $= Just tx
            GL.rotate (angle ! 0) (GL.Vector3 1 0 (0 :: GL.GLfloat))
            GL.rotate (angle ! 1) (GL.Vector3 0 1 (0 :: GL.GLfloat))
            GL.rotate (angle ! 2) (GL.Vector3 0 0 (1 :: GL.GLfloat))
            renderPrimitive Triangles $ forM_  (icosahedron r) $ \(v, t) -> do
                texCoord t
                vertex v
            GL.flush
            GL.texture GL.Texture2D $= GL.Disabled

initialWorld :: IO World
initialWorld = earth >>= \e -> 
                moon >>= \m ->
                return World
    { bodies = [e, m]
    , paused = False
    , debug = False
    , angleX = 0
    , angleY = 0
    , angleZ = 0
    , distance = -100
    , simulation = EarthMoon
    , center = Nothing
    , eyeX = 0
    , eyeY = 0
    , eyeZ = 1
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
    GLUT.createWindow "Gravity"
    windowSize $= Size 1200 1000
    iW <- initialWorld
    worldRef <- newIORef iW
    displayCallback $= display worldRef
    idleCallback $= Just (idle worldRef)
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just (keyboardMouse worldRef)
    depthFunc $= Just Lequal
    depthMask $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    clearColor $= Color4 0 0 0 0
    GLUT.mainLoop

-- | Sets camera and calls rendering for all bodies on the list.
display :: IORef World -> IO ()
display worldRef = do
    clearColor $= Color4 0 0 0 1
    clear [ColorBuffer, DepthBuffer]
-- Set up the view transformation
    matrixMode $= Modelview 0
    loadIdentity
    world <- readIORef worldRef
    let pp = map (V.map (\d -> d / (viewRatio $ simulation world)) . pos) $ bodies world
        viewLimit = length $ bodies world
        d = viewDistance (simulation world) + distance world
    case center world of
        Nothing ->  lookAt (Vertex3 (eyeX world * d) (eyeY world * d) (eyeZ world * d)) 
                    (Vertex3 0 0 0) 
                    (Vector3 0 1 0)

          --lookAt (Vertex3 0 0 (viewDistance $ simulation world)) (Vertex3 0 0 0) (Vector3 0 1 0)
        Just i  -> 
          case i <= viewLimit of
            True -> do 
              let p = pp !! (i-1)
              lookAt (Vertex3 (eyeX world * d) (eyeY world * d) (eyeZ world * d)) 
                    (Vertex3 (p ! 0) (p ! 1) (p ! 2)) 
                    (Vector3 0 1 0)
            False -> lookAt (Vertex3 0 0 (viewDistance $ simulation world)) (Vertex3 0 0 0) (Vector3 0 1 0) -- fallback to centric view
    

  -- Set up the light source
    let ambientLight = Color4 0.1 0.1 0.1 1
        diffuseLight = Color4 0.3 0.3 0.3 1
        specularLight = Color4 1 1 1 1
        lightPosition = Vertex4 100 100 oneUnit 1
    ambient (Light 0) $= ambientLight
    diffuse (Light 0) $= diffuseLight
    specular (Light 0) $= specularLight
    lighting $= Enabled
    light (Light 0) $= Enabled
    position (Light 0) $= lightPosition

    GL.preservingMatrix $ do
--        GL.translate (GL.Vector3 0 0 (distance world))
        --GL.rotate (angleX world) (GL.Vector3 1 0 (0 :: GL.GLfloat))
        --GL.rotate (angleY world) (GL.Vector3 0 1 (0 :: GL.GLfloat))        
        --GL.rotate (angleZ world) (GL.Vector3 0 0 (1 :: GL.GLfloat))    
        mapM_ (\body -> preservingMatrix $ do 
                    drawBody (viewRatio $ simulation world) (radiusRatio $ simulation world) body
            ) (bodies world)
    swapBuffers

idle :: IORef World -> IO () 
idle worldRef = do
    world <- readIORef worldRef
    -- Provide some output for debuging
    when (debug world) (putStrLn $ show $ (pos $ head $ bodies world, velocity $ head $ bodies world))
    unless (paused world) $
        modifyIORef worldRef (\w -> w { bodies = updateSystem deltaTime $ bodies w })
    postRedisplay Nothing

reshape :: Size -> IO () 
reshape size@(Size w h) = do
    viewport $= (Position 0 0, size)
    matrixMode $= Projection
    loadIdentity
    perspective 45 (fromIntegral w / fromIntegral h) 1 10000
    matrixMode $= Modelview 0
    loadIdentity

c5 = cos 0.1
c5n = cos (-0.1)
s5 = sin 0.1
s5n = sin (-0.1)

keyboardMouse :: IORef World -> Key -> KeyState -> Modifiers -> Position -> IO ()
keyboardMouse worldRef (Char 'x') Down _ _ = modifyIORef worldRef (\w -> w { angleX = angleX w + 0.5 
                                                                          ,eyeY = eyeY w * c5 - eyeZ w * s5
                                                                          ,eyeZ = eyeZ w * c5 + eyeY w * s5})
keyboardMouse worldRef (Char 'X') Down _ _ = modifyIORef worldRef (\w -> w { angleX = angleX w - 0.5 
                                                                          ,eyeY = eyeY w * c5n - eyeZ w * s5n
                                                                          ,eyeZ = eyeZ w * c5n + eyeY w * s5n})
keyboardMouse worldRef (Char 'y') Down _ _ = modifyIORef worldRef (\w -> w { angleY = angleY w + 0.5
                                                                          ,eyeX = eyeX w * c5 + eyeZ w * s5
                                                                          ,eyeZ = eyeZ w * c5 - eyeX w * s5})
keyboardMouse worldRef (Char 'Y') Down _ _ = modifyIORef worldRef (\w -> w { angleY = angleY w - 0.5 
                                                                          ,eyeX = eyeX w * c5n + eyeZ w * s5n
                                                                          ,eyeZ = eyeZ w * c5n - eyeX w * s5n})
keyboardMouse worldRef (Char 'z') Down _ _ = modifyIORef worldRef (\w -> w { angleZ = angleZ w + 0.5
                                                                          ,eyeY = eyeY w * c5 + eyeX w * s5
                                                                          ,eyeX = eyeX w * c5 - eyeY w * s5})
keyboardMouse worldRef (Char 'Z') Down _ _ = modifyIORef worldRef (\w -> w { angleZ = angleZ w - 0.5 
                                                                          ,eyeY = eyeY w * c5n + eyeX w * s5n
                                                                          ,eyeX = eyeX w * c5n - eyeY w * s5n})
keyboardMouse worldRef (Char '-') Down _ _ = modifyIORef worldRef (\w -> w { distance = distance w - 100 })
keyboardMouse worldRef (Char '+') Down _ _ = modifyIORef worldRef (\w -> w { distance = distance w + 100 })
keyboardMouse worldRef (Char '0') Down _ _ = modifyIORef worldRef (\w -> w { center = Nothing })
keyboardMouse worldRef (Char '1') Down _ _ = modifyIORef worldRef (\w -> w { center = Just 1 })
keyboardMouse worldRef (Char '2') Down _ _ = modifyIORef worldRef (\w -> w { center = Just 2 })
keyboardMouse worldRef (Char '3') Down _ _ = modifyIORef worldRef (\w -> w { center = Just 3 })
keyboardMouse worldRef (Char '4') Down _ _ = modifyIORef worldRef (\w -> w { center = Just 4 })
keyboardMouse worldRef (Char '5') Down _ _ = modifyIORef worldRef (\w -> w { center = Just 5 })
keyboardMouse worldRef (SpecialKey KeyF1) Down _ _ = 
    earth >>= \e ->
    moon >>= \m ->
    modifyIORef worldRef (\w -> w { bodies = [e, m]
                                , simulation = EarthMoon })
keyboardMouse worldRef (SpecialKey KeyF2) Down _ _ = 
    earth >>= \e ->
    slowMoon >>= \m ->
    modifyIORef worldRef (\w -> w { bodies = [e, m]
                                    , simulation = EarthMoonSlower })
keyboardMouse worldRef (SpecialKey KeyF3) Down _ _ = 
    earth >>= \e ->
    heavyMoon >>= \m ->
    modifyIORef worldRef (\w -> w { bodies = [e, m]
                                    , simulation = EarthMoonHeavier })
keyboardMouse worldRef (SpecialKey KeyF4) Down _ _ = 
    earthLikeHalfMoon >>= \e ->
    moonLike2Earths >>= \m ->
    modifyIORef worldRef (\w -> w { bodies = [e, m]
                                    , simulation = SmallHeavierBigLigher })
keyboardMouse worldRef (SpecialKey KeyF5) Down _ _ = 
    sun >>= \s -> 
    earthWithSun >>= \e ->
    moonWithSun >>= \m ->
    modifyIORef worldRef (\w -> w { bodies = [s, mercury, venus, e, m, jupiter]
                                , simulation = SunEarthMoon })
keyboardMouse worldRef (SpecialKey KeyF12) Down _ _ = modifyIORef worldRef (\w -> w { debug = not $ debug w})
keyboardMouse worldRef (Char ' ') Down _ _ = modifyIORef worldRef (\w -> w { paused = not $ paused w})
keyboardMouse _ (Char 'q') Down _ _ = exitSuccess
keyboardMouse _ _ _ _ _ = return ()

-- | Sets of functions creating list of verices for icosahedron and its texture coordinates.
icosahedron :: GLdouble -> [(GL.Vertex3 GL.GLfloat, GL.TexCoord2 GL.GLfloat)]
icosahedron r = concatMap (map (\v -> ( rV3 v, toTexCoord v))) faces
  where
    faces = subdivide 3 $ map (map toV3 . getVertices) icoFaces2
    toTexCoord (GL.Vertex3 x y z) = GL.TexCoord2 ((atan2 y x / (2*pi)) + 0.5) (acos z / pi)
    toV3 :: [GLfloat] -> Vertex3 GLfloat
    toV3 [x, y, z] = GL.Vertex3 x y z
    rV3 (GL.Vertex3 x y z) = GL.Vertex3 (realToFrac r*x) (realToFrac r*y) (realToFrac r*z)

getVertices :: [Int] -> [[GLfloat]]
getVertices = map (icoVerts !!)

icoFaces :: [[[GLfloat]]]
icoFaces = unfoldr f icoVerts
  where f [] = Nothing
        f (a:b:c:xs) = Just ([a,b,c], xs)

icoFaces2 :: [[Int]]
icoFaces2 = [
    [0, 4, 1],
    [0, 9, 4],
    [9, 5, 4],
    [4, 5, 8],
    [4, 8, 1],
    [8, 10, 1],
    [8, 3, 10],
    [5, 3, 8],
    [5, 2, 3],
    [2, 7, 3],
    [7, 10, 3],
    [7, 6, 10],
    [7, 11, 6],
    [11, 0, 6],
    [0, 1, 6],
    [6, 1, 10],
    [9, 0, 11],
    [9, 11, 2],
    [9, 2, 5],
    [7, 2, 11]
  ]


icoVerts :: [[GLfloat]]
icoVerts = [
    [-x, 0.0, z],
    [x, 0.0, z],
    [-x, 0.0, -z],
    [x, 0.0, -z] ,
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

subdivide :: Int -> [[GL.Vertex3 GL.GLfloat]] -> [[GL.Vertex3 GL.GLfloat]]
subdivide 0 faces = faces
subdivide n faces = subdivide (n-1) $ concatMap subdivideFace faces
  where subdivideFace [a, b, c] = [[a, ab, ac], [ab, b, bc], [ac, bc, c], [ab, ac, bc]]
          where ab = normalizeV3 $ midpoint a b
                ac = normalizeV3 $ midpoint a c
                bc = normalizeV3 $ midpoint b c

normalizeV3 :: GL.Vertex3 GL.GLfloat -> GL.Vertex3 GL.GLfloat
normalizeV3 v@(GL.Vertex3 x y z) = GL.Vertex3 (x/l) (y/l) (z/l)
  where l = sqrt (x*x + y*y + z*z)

midpoint :: GL.Vertex3 GL.GLfloat -> GL.Vertex3 GL.GLfloat -> GL.Vertex3 GL.GLfloat
midpoint (GL.Vertex3 x1 y1 z1) (GL.Vertex3 x2 y2 z2) = GL.Vertex3 ((x1+x2)/2) ((y1+y2)/2) ((z1+z2)/2)

loadTexture :: FilePath -> IO TextureObject
loadTexture filePath = do
  eimg <- JP.readImage filePath
  case eimg of
    Left err -> error err
    Right dynamicImg -> do
      let img = JP.convertRGB8 dynamicImg
      [texName] <- GL.genObjectNames 1
      GL.textureBinding GL.Texture2D GL.$= Just texName
      VS.unsafeWith (JP.imageData img) $ \ptr ->
        GL.build2DMipmaps GL.Texture2D GL.RGB' 
                            (fromIntegral $ JP.imageWidth img) 
                            (fromIntegral $ JP.imageHeight img) . GL.PixelData GL.RGB GL.UnsignedByte $ ptr
      GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Nothing), GL.Linear')
      GL.textureFunction GL.$= GL.Decal
      return texName