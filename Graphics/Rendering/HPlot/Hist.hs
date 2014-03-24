{-# LANGUAGE OverloadedStrings, UnicodeSyntax, BangPatterns, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Rendering.HPlot.Hist (
    hist
    , hist'
    , h_title
    , h_xlab
    , h_xlim
    , h_width
    , h_height
    , h_col
    , h_opacity
    , breaks
    , freedmanDiaconis
    , squareRoot
    , riceRule
    )where

import Data.List
import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Gtk
import Numeric.MathFunctions.Constants (m_epsilon)
import Graphics.Rendering.Chart
import Control.Lens
import Data.Default
import Graphics.Rendering.HPlot.Types
import Graphics.Rendering.HPlot.Bars

type BreakRule = [Double] → Int

data HistOption = HistOption {
    _h_title ∷ String
    , _h_xlab ∷ String
    , _h_ylab ∷ String
    , _h_width ∷ Int
    , _h_height ∷ Int
    , _h_xlim ∷ (Double, Double)
    , _h_col ∷ String
    , _h_opacity ∷ Double
    , _breaks ∷ BreakRule
    }

makeLenses ''HistOption

instance Default HistOption where
    def = HistOption {
        _h_title = []
        , _h_xlab = []
        , _h_ylab = "Frequency"
        , _h_width = 480
        , _h_height = 480
        , _h_xlim = (0, -1)
        , _h_col = "blue"
        , _h_opacity = 1.0
        , _breaks = freedmanDiaconis
    }

convertOpt ∷ HistOption → (PlotOption, BarOption)
convertOpt opt = (
    PlotOption {
        _title = opt^.h_title
        , _xlab = opt^.h_xlab
        , _ylab = opt^.h_ylab
        , _xlim = opt^.h_xlim
        , _width = opt^.h_width
        , _height = opt^.h_height
    },
    BarOption {
        _thickness = 1
        , _opacity = opt^.h_opacity
        , _col = [opt^.h_col]
        , _align = BarsLeft
        , _space = 0
    })

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

freedmanDiaconis ∷ BreakRule
freedmanDiaconis xs = round ((maximum xs - minimum xs) / binSzie)
    where
        binSzie = 2 * iqr xs * n**(-1/3)
        n = fromIntegral $ length xs
        iqr x = let quartile3 = quantile 3 4 x
                    quartile1 = quantile 1 4 x
                in quartile3 - quartile1

squareRoot ∷ BreakRule
squareRoot = round . (sqrt ∷ Double → Double) . fromIntegral . length

riceRule ∷ BreakRule
riceRule xs = ceiling (2*(fromIntegral $ length xs ∷ Double)**(1/3))

hist_ ∷ F.Foldable f ⇒ HistOption → f Double → Layout Double Double
hist_ opt xs' = layout_x_axis.laxis_generate .~ const xAxis $ plot_ popt [bs]
    where
        (popt, bopt) = convertOpt opt
        bs = bars bopt (Just labels, [counts])

        xs = F.toList xs'
        numBins = (opt^.breaks) xs
        labels = autoSteps numBins xs
        counts = V.toList $ histogram_ (length labels - 1) (minimum labels) (maximum labels) (V.fromList xs)

        xAxis ∷ AxisData Double
        xAxis = AxisData {
            _axis_visibility = def,
            _axis_viewport = vmap (min', max'),
            _axis_tropweiv = invmap (min', max'),
            _axis_ticks = [ (v,-5) | v ← newLabels ],
            _axis_grid = labels,
            _axis_labels = [[ (v, show v) | v ← newLabels ]]
            }
                where
                    newLabels = let k = ceiling ((fromIntegral (length labels) / 5) ∷ Double)
                                    l = filter (\(i,_) → (i `mod` k) == 0) $ zip [(1∷Int)..] labels
                                 in snd $ unzip l
                    halfW = (labels!!1 - head labels) / 2
                    min' = minimum labels - halfW
                    max' = maximum labels + halfW

hist ∷ F.Foldable f ⇒ HistOption → f Double → IO ()
hist opt xs = renderableToWindow (toRenderable $ hist_ opt xs) (opt^.h_width) (opt^.h_height)

hist' ∷ F.Foldable f ⇒ HistOption → f Double → String → IO (PickFn ())
hist' opt xs = renderableToFile
    (fo_size .~ (opt^.h_width, opt^.h_height) $ def)
    (toRenderable $ hist_ opt xs)
