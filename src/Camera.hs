module Camera where

import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Vec
import VecGL
import BezierCurve

-- A Camera has a position, view, and up
data Camera = Camera
    { pos  :: !Vec3
    , view :: !Vec3
    , up   :: !Vec3
    } deriving (Show, Eq)

-- Setter for Camera
setCamera px py pz vx vy vz ux uy uz = Camera 
    (Vec3 px py pz)
    (Vec3 vx vy vz)
    (Vec3 ux uy uz)

-- Move forward by amount speed in the x-z plane
cameraMoveForward (Camera p v u) speed =
    let fwdv = normalizeV $ v -. p

        px = x p + x fwdv * speed
        pz = z p + z fwdv * speed

        vx = x v + x fwdv * speed
        vz = z v + z fwdv * speed
    in Camera (Vec3 px (y p) pz) (Vec3 vx (y v) vz) u

cameraRoll (Camera p v u) speed =
    let fwdv = normalizeV $ v -. p in
    Camera p v (rotateV u fwdv speed)

cameraPitch (Camera p v u) speed =
    let fwdv = v -. p
        left = u `cross` fwdv
    in Camera p (rotateV fwdv left speed +. p) (rotateV u left speed)

-- Rotate the camera in the x-z plane
cameraRotate (Camera p v u) speed =
    let fwdv = normalizeV $ v -. p

        vz = z p + sin speed * x fwdv + cos speed * z fwdv
        vx = x p + cos speed * x fwdv - sin speed * z fwdv
    in Camera p (Vec3 vx (y v) vz) u

-- Strafe in the x-z plane
cameraStrafe (Camera p v u) speed =
    let fwdv = normalizeV $ v -. p

        px = x p - z fwdv * speed
        pz = z p + x fwdv * speed

        vx = x v - z fwdv * speed
        vz = z v + x fwdv * speed
    in Camera (Vec3 px (y p) pz) (Vec3 vx (y v) vz) u

-- Simply go up in the y-direction
cameraUp (Camera p v u) speed =
    let py = y p + speed
        vy = y v + speed
    in Camera (Vec3 (x p) py (z p)) (Vec3 (x v) vy (z v)) u

cameraLookAt (Camera p v u) = 
    lookAt (toVertex3 p)
           (toVertex3 v)
           (toVector3 u)

timediv = 10000.0

-- Draw the camera with a Bezier curve path
drawCamera cam bVerts time = do
    (Camera _ v u) <- get cam
    t <- get time
    let p = bezier (t/timediv) bVerts
    cameraLookAt (Camera p v u)

-- Simply place a red dot at the view position
drawCross c = do
    lighting $= Disabled
    color $ Color3 (1.0 :: GLdouble) 0.0 0.0
    renderPrimitive Points $ do
        vertex $ toVertex3 (view c)
    lighting $= Enabled
