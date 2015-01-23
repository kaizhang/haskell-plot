{-# LANGUAGE FlexibleContexts #-}
module Diagrams.Plots.Utils
    ( autoSteps
    , linearMap
    , hasNaN
    , text'
    , drawDendrogram
    , ordering
    , eulerian
    ) where

import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Function
import Graphics.SVGFonts.ReadFont

import Data.Tree
import Data.Maybe
import Data.Clustering.Hierarchical
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.TwoD.Layout.Tree
import Control.Monad.State
import Data.List

import Diagrams.Plots.Types

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

hasNaN :: [(Double, Double)] -> Bool
hasNaN = any (uncurry ((||) `on` isNaN))

text' :: String -> DiaR2
text' s = stroke (textSVG' (TextOpts s lin2 INSIDE_H KERN False 0.2 0.2)) # fc black # lwL 0


dendrogramLayout :: Double -> Double -> Double -> Dendrogram a -> Tree ((Maybe a, Maybe (Colour Double)), P2)
dendrogramLayout w h cut tree = evalState (go tree) 0
  where
    go (Leaf x) = do
        horiz <- get
        put $ horiz + xSep
        return $! Node ((Just x, Just red), w ^& horiz) []
    go (Branch d l r) = do
        left <- go l
        horiz <- get
        put $ horiz + xSep
        right <- go r
        let col | d <= cut = Just red
                | otherwise = Nothing
        return $! Node ((Nothing, col), (w * (maxD - d) / maxD) ^& horiz) [left, right]
    maxD = case tree of
        Branch d _ _ -> d
        Leaf _ -> error "error"
    xSep = h / fromIntegral (2 * ((length.elements) tree - 1))

--dendrogramToBTree :: Dendrogram a -> BTree (Maybe a)
--dendrogramToBTree (Branch _ left right) = BNode Nothing (dendrogramToBTree left) (dendrogramToBTree right)
--dendrogramToBTree (Leaf x) = BNode (Just x) Empty Empty

drawDendrogram :: Double -> Double -> Dendrogram a -> (a -> String) -> Diagram B R2
drawDendrogram w h d show' = renderTree' f g' (dendrogramLayout w h 0.2 d)
  where
    f x | isNothing $ fst x = mempty
        | otherwise = alignL $ text' $ show' $ fromJust $ fst x
    g ((_, col1), a) ((_, col2), b) | ((||) `on` isNothing) col1 col2 = a ~~ b # lwO 1 # dashingO [4, 2] 1
                                    | otherwise = a ~~ b # lwO 1 # lc (fromJust col1)
    g' ((_, col1), a) ((_, col2), b) | ((||) `on` isNothing) col1 col2 = l # dashingO [4, 2] 1
                                     | otherwise = l # lc (fromJust col1)
      where
        (x1, y1) = unp2 a
        (x2, y2) = unp2 b
        l = fromVertices [x1 ^& y1, x1 ^& y2, x2 ^& y2] # lwO 1

-- | reorder data
ordering :: [a] -> (a -> a -> Double) -> [a]
ordering xs f = elements $ dendrogram UPGMA xs f

-- | eulerian distance
eulerian :: [Double] -> [Double] -> Double
eulerian xs ys = sqrt $ foldl' (+) 0 $ zipWith (\x y -> (x - y)**2) xs ys

