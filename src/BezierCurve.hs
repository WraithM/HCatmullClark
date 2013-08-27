module BezierCurve where

import Vec

-- Binomial coefficient
choose :: (Integral a) => a -> a -> a
choose n k = product [k+1..n] `div` product [1..n-k]

-- Calculate the coefficient for the terms in the Bezier curve
bezierCoeff :: (Integral a) => a -> a -> Double -> Double
bezierCoeff i n t = fromIntegral (n `choose` i) * (t ^ i) * ((1 - t) ^ (n - i))

-- Calculate the Bezier curve as a function of time
bezier :: Double -> [Vec3] -> Vec3
bezier t (p0:pts) = p0 +. (sumV . zipWith (\i p -> p *. bezierCoeff i n t) [1..]) pts
    where n = length pts
