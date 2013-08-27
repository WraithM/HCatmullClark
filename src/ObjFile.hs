module ObjFile where

import Graphics.Rendering.OpenGL

import Vec
import VecGL

-- Read a GLdouble from a string
readGLdouble :: String -> GLdouble
readGLdouble s = realToFrac (read s :: Double)

-- A face command from the Obj file
data FaceCommand = FaceCommand [Int] deriving (Show)

-- The file contains faces and vertices
data ObjCommand = ObjF FaceCommand | ObjV (Vertex3 GLdouble) deriving (Show)

-- Read a line from an Obj file
readCommand :: [String] -> ObjCommand
readCommand ("f" : xs) = ObjF (FaceCommand $ map read xs)
readCommand ("v" : x : y : z : _) = ObjV (Vertex3 (readGLdouble x) (readGLdouble y) (readGLdouble z))
readCommand  _  = ObjF (FaceCommand []) -- Default to face

-- Helper functions for reading the commands
isVert (ObjV _) = True
isVert _        = False
isFace (ObjF _) = True
isFace _        = False
commandToVert (ObjV v) = v
commandToFace (ObjF f) = f
faceIndecies (FaceCommand xs) = xs

-- Separate the verts and faces from the commands
commandsToVsFs :: [ObjCommand] -> ([Vertex3 GLdouble], [FaceCommand])
commandsToVsFs xs = (getVerts xs, getFaces xs)
    where getVerts = map commandToVert . filter isVert
          getFaces = map commandToFace . filter isFace

-- Read an Obj file
readObjFile :: FilePath -> IO [ObjCommand]
readObjFile filename = do 
    file <- readFile filename
    return $ map readCommand (lw file)
    where lw = map words . lines


----- Code not related to reading Obj file

-- Simply get the vertices for the faces
getFace :: [Vertex3 GLdouble] -> FaceCommand -> (Vec3, Vec3, Vec3)
getFace vs (FaceCommand (x:y:z:_)) =
    let p1 = toVec3 $ vs !! (x - 1)
        p2 = toVec3 $ vs !! (y - 1)
        p3 = toVec3 $ vs !! (z - 1)
    in (p1, p2, p3)
getFace _ _ = (zeroVec, zeroVec, zeroVec)

-- Calculate the face normal after using getFace
calcFaceNormal :: (Vec3, Vec3, Vec3) -> Vec3
calcFaceNormal (p1,p2,p3) =
    let v1 = p2 -. p1
        v2 = p3 -. p1
    in normalizeV (v1 `cross` v2)

calcFaceNormals :: [Vertex3 GLdouble] -> [FaceCommand] -> [Vec3]
calcFaceNormals vs = map (calcFaceNormal . getFace vs)

-- This calculates the vertex normal from the face normals
-- I simply find all of the faces which contain that vertex, then I average them
calcVertexNormals :: [Vertex3 GLdouble] -> [FaceCommand] -> [Normal3 GLdouble]
calcVertexNormals vs fs =
    let containsVert vi (FaceCommand xs) = elem vi xs
        getAdjFaces vi = filter (containsVert vi)
        vertIndecies = [0..(length vs - 1)]

        adjFaces = map (\vi -> getAdjFaces vi fs) vertIndecies
        adjNormals = map (calcFaceNormals vs) adjFaces
    in map (toNormal3 . normalizeV . meanV) adjNormals
