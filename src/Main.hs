module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Exit (exitSuccess)
import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Applicative
import qualified Data.Vector as V

import Vec
import VecGL (toVec3, toNormal3)
import Camera
import ObjFile
import Model
import CatmullClark

-- Draw a green grid
drawGrid = do
    lighting $= Disabled
    color $ (Color4 (0.0 :: GLdouble) 0.7 0.2 1.0)
    renderPrimitive Lines $ do
        forM_ [-500,-495..500] $ \i -> do
            vertex $ Vertex3 (-500 :: GLdouble) 0 i
            vertex $ Vertex3 (500  :: GLdouble) 0 i
            vertex $ Vertex3 (i    :: GLdouble) 0 (-500)
            vertex $ Vertex3 (i    :: GLdouble) 0 500
    lighting $= Enabled

orange = Color4 (0.6 :: GLfloat) 0.2 0.0 1.0

-- Main display function
display cam bezierVerts time angle blockI = do
    clear [ColorBuffer, DepthBuffer]
    loadIdentity
    drawCamera cam bezierVerts time
    drawGrid
    preservingMatrix $ do
        a <- get angle
        b <- get blockI

        -- Lighting
        position light0 $= lightPosition

        -- Draw blockI
        materialAmbientAndDiffuse Front $=! orange
        color orange
        rotate a $ Vector3 0.0 1.0 0.0
        translate $ Vector3 (0.0 :: GLdouble) 1.0 0.0
        drawModel b
    swapBuffers
    flush

-- Q to quit,
-- R to rotate the block I
-- E pauses the camera
keyboard _ _ (Char 'q') Down = exitSuccess
keyboard angle _ (Char 'r') Down = do
    a <- get angle
    angle $=! (a + 1.0)
keyboard _ paused (Char 'e') Down = do
    p <- get paused
    paused $=! not p
keyboard _ _ _ _ = return ()

-- W increases the number of subdivisions
keyboardBlockI blockI (Char 'w') Down = do
    b <- get blockI
    blockI $=! facesToModel (catmullClark (modelToFaces b))
keyboardBlockI _ _ _ = return ()

-- This handles the input for the camera, blockI, and misc.
keyboardMouse angle paused blockI key state mod pos = do
    keyboard angle paused key state
    keyboardBlockI blockI key state

-- Window reshape
reshape s@(Size w h) = do
    viewport $= (Position 0 0, s)
    postRedisplay Nothing

-- Animation function
idle t paused = do 
    threadDelay 10000
    milliseconds <- fromIntegral <$> get elapsedTime
    p <- get paused
    if not p then t $=! milliseconds else return ()
    postRedisplay Nothing

-- Light properties
light0        = Light 0 

lightPosition = Vertex4 3.0 7.0 3.0 1.0

lightWhite    = Color4 1.0 1.0 1.0 1.0
lightBlue     = Color4 0.0 0.0 1.0 1.0
lightSpecular = Color4 0.8 0.8 0.8 1.0
lightAmbient  = Color4 0.2 0.2 0.2 1.0

shininess     = 60.0

initFunc = do
    -- Enable light
    lighting $= Enabled
    light light0 $= Enabled

    -- Black background and depth
    clearColor $= Color4 0.0 0.0 0.0 0.0
    clearDepth $= 1
    shadeModel $= Smooth

    -- Material properties
    materialSpecular  Front $= lightSpecular
    materialShininess Front $= shininess

    -- Light
    position light0 $= lightPosition
    diffuse  light0 $= lightWhite
    specular light0 $= lightAmbient
    ambient  light0 $= lightAmbient

    lightModelAmbient $= lightAmbient
    
    hint PerspectiveCorrection $= Nicest
    depthFunc $= Just Less
    pointSize $= 3.0

    -- Set to full screen, disable cursor
    fullScreenToggle
    --cursor $= None

    -- Set perspective and load modelview
    matrixMode $= Projection
    loadIdentity

    (Size w h) <- get windowSize
    perspective 80.0 (realToFrac w / realToFrac h) 0.1 300.0

    matrixMode $= Modelview 0
    loadIdentity

main :: IO ()
main = do
    -- Read I and Bezier curve from file
    x <- readObjFile "extrudeI.obj"
    y <- readObjFile "bezier.obj"

    -- Get the vertecies, faces, and vertex normals
    let (vs', fs') = commandsToVsFs x
    let ns' = map toNormal3 $ calcFaceNormals vs' fs'

    -- Get bezier points
    let bezierVerts = (map toVec3 . fst . commandsToVsFs) y

    -- Convert everything to vectors
    let fs = V.fromList $ map faceIndecies fs'

    let vs = V.fromList vs'
    let ns = V.fromList ns'

    -- Initialize globals
    -- The Camera, time, pausing the motion of the camera, angle of the I,
    -- and the model
    camera <- newIORef $ setCamera 0.0 2.5 5.0 0.0 2.5 0.0 0.0 1.0 0.0
    time   <- newIORef (0.0 :: Double)
    paused <- newIORef False
    angle  <- newIORef (0.0 :: GLdouble)
    blockI <- newIORef $ (Model vs ns, fs)

    -- Start OpenGL/GLUT
    (progname, _) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, RGBMode, WithDepthBuffer]
    createWindow "OpenGL"

    -- Declare callback functions
    displayCallback $= (display camera bezierVerts time angle blockI)
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just (keyboardMouse angle paused blockI)
    idleCallback $= Just (idle time paused)

    -- Run
    initFunc
    mainLoop

