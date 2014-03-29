{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Graphics.Rendering.HPlot.Utils (
        mkColor
      , freedmanDiaconis
      , squareRoot
      , riceRule
      , histogram_
    ) where

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Numeric.MathFunctions.Constants (m_epsilon)
import Data.Colour
import Data.Colour.Names
import Data.Maybe
import Data.List

-- Adapted from http://hackage.haskell.org/package/statistics-0.11.0.0/docs/Statistics-Quantile.html
quantile ∷ Int        -- ^ /k/, the desired quantile.
           -> Int        -- ^ /q/, the number of quantiles.
           -> [Double]   -- ^ /x/, the sample data.
           -> Double
quantile k q x
  | any isNaN x   = error "Sample contains NaNs"
  | n == 1          = head x
  | q < 2           = error "At least 2 quantiles is needed"
  | k < 0 || k >= q = error "Wrong quantile number"
  | otherwise       = xj + g * (xj1 - xj)
  where
    j   = floor idx
    idx = fromIntegral (n - 1) * fromIntegral k / fromIntegral q
    g   = idx - fromIntegral j
    xj  = sx !! j
    xj1 = sx !! (j+1)
    sx  = sort x
    n   = length x

histogram_ :: (Num b, RealFrac a, G.Vector v0 a, G.Vector v1 b) =>
              Int
           -- ^ Number of bins.  This value must be positive.  A zero
           -- or negative value will cause an error.
           -> a
           -- ^ Lower bound on interval range.  Sample data less than
           -- this will cause an error.
           -> a
           -- ^ Upper bound on interval range.  This value must not be
           -- less than the lower bound.  Sample data that falls above
           -- the upper bound will cause an error.
           -> v0 a
           -- ^ Sample data.
           -> v1 b
histogram_ numBins lo hi xs0 = G.create (GM.replicate numBins 0 >>= bin xs0)
  where
    bin xs bins = go 0
     where
       go i | i >= len = return bins
            | otherwise = do
         let x = xs `G.unsafeIndex` i
             b = truncate $ (x - lo) / d
         GM.write bins b . (+1) =<< GM.read bins b
         go (i+1)
       len = G.length xs
       d = ((hi - lo) * (1 + realToFrac m_epsilon)) / fromIntegral numBins

mkColor ∷ String → Double → AlphaColour Double
mkColor c = withOpacity (fromMaybe blue $ readColourName c)

freedmanDiaconis, squareRoot, riceRule ∷ [Double] → Int
freedmanDiaconis xs = round ((maximum xs - minimum xs) / binSzie)
    where
        binSzie = 2 * iqr xs * n**(-1/3)
        n = fromIntegral $ length xs
        iqr x = let quartile3 = quantile 3 4 x
                    quartile1 = quantile 1 4 x
                in quartile3 - quartile1

squareRoot = round . (sqrt ∷ Double → Double) . fromIntegral . length

riceRule xs = ceiling (2*(fromIntegral $ length xs ∷ Double)**(1/3))
