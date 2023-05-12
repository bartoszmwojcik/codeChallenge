{-# LANGUAGE RecordWildCards #-}

module Main where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT as GLUT
import Data.IORef
import System.Exit
import Control.Monad (unless)
import Data.Array (Array, listArray, bounds, (!), (//), accumArray, assocs)

type Coordinate = (Int, Int, Int)
type Element = Int 
type ThreeDArray = Array Coordinate Element

createThreeDArray :: Int -> ThreeDArray
createThreeDArray bound = listArray bounds initialValues
  where
    bounds = ((0, 0, 0), (bound-1, bound-1, bound-1))
    initialValues = replicate (bound * bound * bound) 0 

data World = World
    { boxes :: [GL.Vector3 GL.GLfloat]
    , bits :: ThreeDArray
    , side :: Float 
    , size :: Int 
    , paused :: Bool
    , angleX :: GL.GLfloat
    , angleY :: GL.GLfloat
    , distance :: GL.GLfloat
    }

initialWorld :: World
initialWorld = World
    { boxes = [GL.Vector3 0 0 0]
    , bits = createThreeDArray 1 // [((0,0,0), 1)]
    , side = 400
    , size = 1
    , paused = False
    , angleX = 0
    , angleY = 0
    , distance = -100
    }

blue,red,green,yellow,purple,white,black :: Color4 GLfloat
blue   = Color4 0   0   1   1
red    = Color4 1   0   0   1
green  = Color4 0   1   0   1
yellow = Color4 1   1   0   1
white  = Color4 1   1   1   1
black  = Color4 0   0   0   1
purple = Color4 0.5 0   0.5 1

main :: IO ()
main = do
    (progname, _args) <- GLUT.getArgsAndInitialize
    initialDisplayMode $= [GLUT.DoubleBuffered, GLUT.RGBMode, GLUT.WithDepthBuffer]
    GLUT.createWindow "3D Box"
    windowSize $= Size 800 800
    worldRef <- newIORef initialWorld
    displayCallback $= display worldRef
    idleCallback $= Just (idle worldRef)
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just (keyboardMouse worldRef)
    depthFunc $= Just Lequal
    depthMask $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    clearColor $= Color4 1 1 1 0
    --materialSpecular Front $= red
    let ambientMaterial = Color4 0 0 0 1
        diffuseMaterial = Color4 0 0 0 1  -- Red color
        specularMaterial = Color4 1 0 0 0  -- No specular reflection
    --    shininessMaterial = 1

    materialAmbient Front $= ambientMaterial
    materialDiffuse Front $= diffuseMaterial
    materialSpecular Front $= Color4 0 0 0 0
    materialShininess Front $= 2
    --shininess Front $= shininessMaterial
--    GL.lighting $= Enabled
--    GL.light (GL.Light 0) $= Enabled
--    let ambientLight = GL.Color4 0.2 0.2 0.2 1
--        diffuseLight = white
--        specularLight = red
--        lightPosition = GL.Vertex4 1000 0 0 1
    --GL.ambient (GL.Light 0) $= ambientLight
--    GL.diffuse (GL.Light 0) $= diffuseLight
    --GL.specular (GL.Light 0) $= specularLight
--    GL.position (GL.Light 0) $= lightPosition

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
        diffuseLight = Color4 0.1 0.1 0.1 1
        specularLight = Color4 1 1 1 1
        lightPosition = Vertex4 (-0) 0 1000 3
    ambient (Light 0) $= ambientLight
    diffuse (Light 0) $= diffuseLight
    specular (Light 0) $= specularLight
    lighting $= Enabled
    light (Light 0) $= Enabled

    world <- readIORef worldRef
    GL.preservingMatrix $ do
        GL.translate (GL.Vector3 0 0 (distance world))
        GL.rotate (angleX world) (GL.Vector3 1 0 (0 :: GL.GLfloat))
        GL.rotate (angleY world) (GL.Vector3 0 1 (0 :: GL.GLfloat))        
        position (Light 0) $= lightPosition
        mapM_ (\boxCenter -> GL.preservingMatrix $ do
                                GL.translate boxCenter
                                drawBox (0.95 * side world) red black
            ) (boxes world)
    swapBuffers

vector3X, vector3Y, vector3Z :: GL.Vector3 GLfloat -> GLfloat
vector3X (GL.Vector3 x _ _) = x
vector3Y (GL.Vector3 _ y _) = y
vector3Z (GL.Vector3 _ _ z) = z


idle :: IORef World -> IO ()
idle worldRef = do
    world <- readIORef worldRef
    unless (paused world) $
        modifyIORef worldRef (\w -> w { angleX = angleX w + 0.3, angleY = angleY w + 0.1 })
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
keyboardMouse worldRef (Char ' ') Down _ _ = modifyIORef worldRef (\w -> w { paused = not (paused w) })
keyboardMouse worldRef (Char '-') Down _ _ = modifyIORef worldRef (\w -> w { distance = distance w - 100 })
keyboardMouse worldRef (Char '+') Down _ _ = modifyIORef worldRef (\w -> w { distance = distance w + 100 })
keyboardMouse worldRef (SpecialKey KeyF1) Down _ _ = modifyIORef worldRef splitBoxes
keyboardMouse _ (Char 'q') Down _ _ = exitSuccess
keyboardMouse _ _ _ _ _ = return ()

splitBoxes :: World -> World
splitBoxes world@World {..} =
    world { size = size + 1
          , side = side / 3
          , bits = nBits
          , boxes = bites2boxes nBits}
    where 
        newBits :: ThreeDArray -> Int -> ThreeDArray
        newBits bs s = accumArray (+) 0 ((0,0,0), (3^s-1,3^s-1,3^s-1)) $ concat $ map asoc2asoc (assocs bs)
        nBits = newBits bits size

        asoc2asoc :: (Coordinate,Element) -> [(Coordinate,Element)]
        asoc2asoc ((x,y,z),1) = [((3*x,3*y,3*z),1), ((3*x+2,3*y,3*z),1)
                                , ((3*x,3*y+2,3*z),1), ((3*x,3*y,3*z+2),1)
                                , ((3*x+2,3*y+2,3*z),1), ((3*x+2,3*y,3*z+2),1)
                                , ((3*x,3*y+2,3*z+2),1), ((3*x+2,3*y+2,3*z+2),1)
                                , ((3*x+1,3*y,3*z),1), ((3*x+1,3*y+2,3*z),1)
                                , ((3*x,3*y+1,3*z),1), ((3*x+2,3*y+1,3*z),1)
                                , ((3*x+1,3*y,3*z+2),1), ((3*x+1,3*y+2,3*z+2),1)
                                , ((3*x,3*y+1,3*z+2),1), ((3*x+2,3*y+1,3*z+2),1)
                                , ((3*x,3*y,3*z+1),1), ((3*x,3*y+2,3*z+1),1)
                                , ((3*x+2,3*y,3*z+1),1), ((3*x+2,3*y+2,3*z+1),1)
                                ]
        asoc2asoc ((x,y,z),_) = []

        bites2boxes :: ThreeDArray -> [GL.Vector3 GL.GLfloat]
        bites2boxes bs = map mapBit2Box $ filter (\a -> snd a == 1) $ assocs bs
            where 
                mapBit2Box :: (Coordinate, Element) -> GL.Vector3 GL.GLfloat
                mapBit2Box ((x,y,z),val) = GL.Vector3 (cXYZ x) (cXYZ y) (cXYZ z)
                s = (3^size) `div` 2
                cXYZ a = side * fromIntegral (a - s) / 3

splitBox :: GL.Vector3 GL.GLfloat -> [GL.Vector3 GL.GLfloat]
splitBox (GL.Vector3 cx cy cz) =
    [ GL.Vector3 (cx + x * newSize) (cy + y * newSize) (cz + z * newSize)
    | x <- offsets, y <- offsets, z <- offsets ]
  where
    --newSize = 400 / 3
    newSize = (abs cx + abs cy + abs cz) / 3
    offsets = [-newSize, 0, newSize]

drawBox :: GLfloat -> Color4 GLfloat -> Color4 GLfloat -> IO ()
drawBox size faceColor edgeColor = do
    let s2 = size / 2
    renderPrimitive Quads $ do
        color faceColor
        materialSpecular Front $= Color4 1 0 0 0
        materialShininess Front $= 2
        -- Front face
        vertex $ Vertex3 (-s2) (-s2) s2
        vertex $ Vertex3 s2 (-s2) s2
        vertex $ Vertex3 s2 s2 s2
        vertex $ Vertex3 (-s2) s2 s2
        -- Back face
        vertex $ Vertex3 (-s2) (-s2) (-s2)
        vertex $ Vertex3 (-s2) s2 (-s2)
        vertex $ Vertex3 s2 s2 (-s2)
        vertex $ Vertex3 s2 (-s2) (-s2)
        -- Left face
        vertex $ Vertex3 (-s2) (-s2) (-s2)
        vertex $ Vertex3 (-s2) (-s2) s2
        vertex $ Vertex3 (-s2) s2 s2
        vertex $ Vertex3 (-s2) s2 (-s2)
        -- Right face
        vertex $ Vertex3 s2 (-s2) (-s2)
        vertex $ Vertex3 s2 s2 (-s2)
        vertex $ Vertex3 s2 s2 s2
        vertex $ Vertex3 s2 (-s2) s2
        -- Top face
        vertex $ Vertex3 (-s2) s2 (-s2)
        vertex $ Vertex3 (-s2) s2 s2
        vertex $ Vertex3 s2 s2 s2
        vertex $ Vertex3 s2 s2 (-s2)
        -- Bottom face
        vertex $ Vertex3 (-s2) (-s2) (-s2)
        vertex $ Vertex3 s2 (-s2) (-s2)
        vertex $ Vertex3 s2 (-s2) s2
        vertex $ Vertex3 (-s2) (-s2) s2
    color edgeColor
    materialSpecular Front $= Color4 1 1 1 1
    materialShininess Front $= 100
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



