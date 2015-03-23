{-# LANGUAGE FlexibleContexts #-}
module Diagrams.Plots.Utils
    ( projection
    , autoSteps
    , linearMap
    , linearMapBound
    , hasNaN
    , text'
    ) where

import Data.Ord (comparing)
import Data.Function
import Graphics.SVGFonts.ReadFont

import Diagrams.Prelude
import Data.List

import Diagrams.Plots.Types
import Diagrams.Backend.Cairo.Text

-- | project a 3d point to 2d
projection :: (Double, Double, Double)  -- ^ position of camera
           -> (Double, Double, Double)  -- ^ orientation of camera
           -> (Double, Double, Double)  -- ^ viewer's position
           -> (Double, Double, Double)  -- ^ 3d point to be projected
           -> (Double, Double)
projection (cx',cy',cz') (θx,θy,θz) (ex,ey,ez) (ax,ay,az) = (bx, by)
  where
    bx = ez / dz * dx - ex
    by = ez / dz * dy - ey
    dx = cy * (sz * y + cz * x) - sy * z
    dy = sx * (cy * z + sy * (sz * y + cz * x)) + cx * (cz * y - sz * x)
    dz = cx * (cy * z + sy * (sz * y + cz * x)) - sx * (cz * y - sz * x)
    x = ax - cx'
    y = ay - cy'
    z = az - cz'
    sx = sin θx
    sy = sin θy
    sz = sin θz
    cx = cos θx
    cy = cos θy
    cz = cos θz

chooseStep :: RealFloat a => a -> (a,a) -> Rational
chooseStep nsteps (x1,x2) = minimumBy (comparing proximity) stepVals
  where
    delta = x2 - x1
    mult  = 10 ^^ ((floor $ logBase 10 $ delta / nsteps)::Integer)
    stepVals = map (mult*) [0.1,0.2,0.25,0.5,1.0,2.0,2.5,5.0,10,20,25,50]
    proximity x = abs $ delta / realToFrac x - nsteps
{-# INLINE chooseStep #-}

-- | Given a target number of values, and a list of input points,
--   find evenly spaced values from the set {1*X, 2*X, 2.5*X, 5*X} (where
--   X is some power of ten) that evenly cover the input points.
autoSteps :: Int -> (Double, Double) -> (Rational, Rational, Rational)
autoSteps nSteps (minV, maxV) = (min', max', step)
  where
    r@(minV', maxV')  | minV == maxV = (minV-0.5,minV+0.5)
                      | otherwise    = (minV, maxV)
    step = chooseStep (fromIntegral nSteps) r
    min' = fromIntegral (floor   $ realToFrac minV' / step :: Integer) * step
    max' = fromIntegral (ceiling $ realToFrac maxV' / step :: Integer) * step

linearMap :: (Double, Double) -> (Double, Double) -> PointMap Double
linearMap (l, u) (l', u') = PointMap mapFn (l, u)
  where
    mapFn x | x < l || x > u = Nothing
            | otherwise = Just $ (x - l) / (u - l) * (u' - l') + l'
{-# INLINE linearMap #-}

linearMapBound :: (Double, Double) -> (Double, Double) -> PointMap Double
linearMapBound (l, u) (l', u') = PointMap mapFn (l, u)
  where
    mapFn x | x < l = Just l' 
            | x > u = Just u'
            | otherwise = Just $ (x - l) / (u - l) * (u' - l') + l'
{-# INLINE linearMapBound #-}

hasNaN :: [(Double, Double)] -> Bool
hasNaN = any (uncurry ((||) `on` isNaN))

--text' :: Double -> String -> DiaR2
--text' size str = stroke (textSVG' (TextOpts str lin2 INSIDE_WH HADV False size size)) # fc black # lwL 0

--text' :: Double -> String -> DiaR2
--text' size str = textVisualBounded (fontSize (Local size) mempty) str # fontSize (Local size)

text' :: Double -> String -> DiaR2
text' size str = text str # fontSizeL size
