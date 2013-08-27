module Vec where

-- A simple vector typeclass
class Vec a where
    dot   :: a -> a -> Double -- Dot and cross products
    cross :: a -> a -> a
    (+.)  :: a -> a -> a      -- Vector addition and subraction
    (-.)  :: a -> a -> a
    (*.)  :: a -> Double -> a -- Scalar multiplication and division
    (/.)  :: a -> Double -> a

-- Calculate the magnitude of a vector
magnitude :: Vec a => a -> Double
magnitude v = sqrt $ v `dot` v

-- Normalize vectors
normalizeV :: Vec a => a -> a
normalizeV v = v /. (magnitude v)

-- Rotate a vector p about the axis v' by the amount angle
rotateV :: Vec a => a -> a -> Double -> a
rotateV p v' angle =
    let v = normalizeV v'
        o = v *. (p `dot` v)
        a = p -. o
        b = v `cross` a
    in o +. a *. cos angle +. b *. sin angle

-- Take the average, component wise, of a list of vector
meanV :: [Vec3] -> Vec3
meanV = uncurry (/.) . foldl (\(s, c) e -> (e +. s, c+1)) (zeroVec, 0)

sumV :: [Vec3] -> Vec3
sumV = foldl (\s e -> e +. s) zeroVec

-- A 3-vector
data Vec3 = Vec3 { x :: !Double, y :: !Double, z :: !Double } deriving (Show, Eq)

-- The zero vector
zeroVec = Vec3 0.0 0.0 0.0

-- An instance of the vector typeclass, as defined above
instance Vec Vec3 where
    (Vec3 x1 y1 z1) `dot` (Vec3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2
    (Vec3 x1 y1 z1) `cross` (Vec3 x2 y2 z2) =
        let x = y1*z2 - z1*y2
            y = z1*x2 - x1*z2
            z = x1*y2 - y1*x2
        in Vec3 x y z
    (Vec3 x1 y1 z1) +. (Vec3 x2 y2 z2) = Vec3 (x1+x2) (y1+y2) (z1+z2)
    (Vec3 x1 y1 z1) -. (Vec3 x2 y2 z2) = Vec3 (x1-x2) (y1-y2) (z1-z2)
    (Vec3 x y z) *. c = Vec3 (x*c) (y*c) (z*c)
    (Vec3 x y z) /. c = Vec3 (x/c) (y/c) (z/c)
