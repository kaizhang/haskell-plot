{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Graphics.Rendering.HPlot.Utils
    ( autoSteps
    , linearMap
    ) where

import Data.List (minimumBy)
import Data.Ord (comparing)
import Graphics.Rendering.HPlot.Types

chooseStep :: RealFloat a => a -> (a,a) -> Rational
{-# INLINE chooseStep #-}
chooseStep nsteps (x1,x2) = minimumBy (comparing proximity) stepVals
  where
    delta = x2 - x1
    mult  = 10 ^^ ((floor $ logBase 10 $ delta / nsteps)::Integer)
    stepVals = map (mult*) [0.1,0.2,0.25,0.5,1.0,2.0,2.5,5.0,10,20,25,50]
    proximity x = abs $ delta / realToFrac x - nsteps

-- | Given a target number of values, and a list of input points,
--   find evenly spaced values from the set {1*X, 2*X, 2.5*X, 5*X} (where
--   X is some power of ten) that evenly cover the input points.
autoSteps ∷ Int -> (Double, Double) -> (Double, Double, Double)
autoSteps nSteps (minV, maxV) = (fromRational min', fromRational max', fromRational step)
  where
    r@(minV', maxV')  | minV == maxV = (minV-0.5,minV+0.5)
                      | otherwise    = (minV, maxV)
    step = chooseStep (fromIntegral nSteps) r
    min' = fromIntegral (floor   $ realToFrac minV' / step ∷ Integer) * step
    max' = fromIntegral (ceiling $ realToFrac maxV' / step ∷ Integer) * step

linearMap ∷ (Double, Double) → (Double, Double) → PointMap Double
linearMap (l, u) (l', u') = PointMap mapFn (l, u)
  where
    mapFn x | x < l || x > u = Nothing
            | otherwise = Just $ (x - l) / (u - l) * (u' - l') + l'
