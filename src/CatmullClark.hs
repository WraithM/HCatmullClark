module CatmullClark where

import qualified Data.Vector as V (Vector, fromList, toList, map, (!))
import Data.List (nub)
import Data.Maybe (catMaybes)

import Vec
import VecGL (toVec3, toVertex3, toNormal3)
import Model
import ObjFile

-- Internal data types for the Catmull Clark algorithm
type Point = Vec3
data Face = Face [Point] deriving (Eq, Show)
data Edge = Edge Point Point deriving (Eq, Show)

-- Convert between model and internal data
modelToFaces :: (Model, V.Vector [Int]) -> [Face]
modelToFaces (Model vs _, fs) = V.toList $ V.map (\is -> Face (map toVec3 $ fromIndecies vs is)) fs
    where fromIndecies xs = map (\i -> xs V.! (i - 1))

facesToModel :: [Face] -> (Model, V.Vector [Int])
facesToModel faces =
    let verts = nub . concat $ map (\(Face pl) -> pl) faces
        vertDict = zip verts [1..]

        fs = map (\(Face pl) -> FaceCommand $ catMaybes $ map (\v -> lookup v vertDict) pl) faces
        vs = map toVertex3 verts
        ns = map toNormal3 $ calcFaceNormals vs fs
    in (Model (V.fromList vs) (V.fromList ns), (V.fromList $ map faceIndecies fs))

-- Calculate the centroid of a Face or Edge
centroid = meanV
midEdge (Edge p1 p2) = meanV [p1,p2]

-- Helper functions relating to finding points
pointInFace p (Face pl) = elem p pl
pointInEdge p (Edge p1 p2) = p == p1 || p == p2
edgeInFace (Edge p1 p2) face = pointInFace p1 face && pointInFace p2 face

-- Find adjacent faces and edges
edgeAdjFaces e = filter (edgeInFace e)
pointAdjEdges p = filter (pointInEdge p)
pointAdjFaces p = filter (pointInFace p)

-- Calculate the face point and edge point
facePoint (Face pl) = centroid pl
edgePoint faces e =
    let fps = map facePoint (edgeAdjFaces e faces) in
    centroid [midEdge e, centroid fps]

-- Modify the original points
modPoint faces edges p =
    let pEdges = pointAdjEdges p edges
        pFaces = pointAdjFaces p faces
        n = fromIntegral $ length pFaces
        
        avgFace = centroid $ map facePoint pFaces
        avgEdge = centroid $ map midEdge pEdges

    in (avgFace +. (avgEdge *. 2.0) +. (p *. (n - 3.0))) /. n

-- Find all of the edges in a face
edges (Face pl) = nub [ Edge p1 p2 | p1 <- pl, p2 <- pl, p1 /= p2 ]

-- Do one iteration of the catmullClark algorithm
catmullClark :: [Face] -> [Face]
catmullClark faces =
    let elist = concat $ map edges faces

        modFace face@(Face pl) =
            let fp = facePoint face
                eps = (map (edgePoint faces) . edges) face
                pl' = map (modPoint faces elist) pl

            in nub [ Face [e, p, e', fp] | 
                e <- eps, p <- pl', e' <- eps ]
    in concat $ map modFace faces

