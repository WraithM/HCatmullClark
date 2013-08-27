module Model where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import qualified Data.Vector as V
import Control.Monad

-- Data structure for holding the model
-- We have vertex, normal, and texture data
data Model = Model
    { vertices :: !(V.Vector (Vertex3   GLdouble))
    , normals  :: !(V.Vector (Normal3   GLdouble))
    } deriving (Show, Eq)

-- Render the vertices and normals
renderVNT vs ns = do
    let vns = zip vs ns
    forM_ vns $ \(v,n) -> do
        normal n
        vertex v

-- Draw the model
drawModel (Model vs ns, fs) = do
    V.forM_ fs $ \is -> do
        let vs' = fromIndecies vs is
        let ns' = fromIndecies ns is
        renderPrimitive Triangles $ do
            renderVNT vs' ns'
    where fromIndecies xs = map (\i -> xs V.! (i - 1))
