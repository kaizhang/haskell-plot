{-# LANGUAGE OverloadedStrings, UnicodeSyntax, BangPatterns, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Rendering.HPlot.Hist (
    hist
    , hist'
    , hist_common
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
import Numeric.MathFunctions.Constants (m_epsilon)
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Gtk
import Control.Lens
import Data.Default
import Graphics.Rendering.HPlot.Type
import Control.Applicative

type BreakRule = [Double] → Int

data HistOption = HistOption {
    _hist_common ∷ PlotOption
    , _breaks ∷ BreakRule
    }

makeLenses ''HistOption

instance Default HistOption where
    def = HistOption {
        _hist_common = ylab .~ "Frequency" $ def
        , _breaks = freedmanDiaconis
    }

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

hist_ ∷ F.Foldable f ⇒ f Double → HistOption → Layout Double Double
hist_ xs' opt = layout
    where
        layout = 
            layout_title .~ opt^.hist_common^.title
            $ layout_plots .~ [plotBars bars]
            $ layout_x_axis . laxis_generate .~ const xAxis
            $ layout_x_axis . laxis_title .~ opt^.hist_common.xlab
            $ layout_y_axis . laxis_title .~ "Frequency"
            $ layout_bottom_axis_visibility.axis_show_ticks .~ False
            $ def ∷ Layout Double Double
        bars = 
            plot_bars_values .~ zip labels (return <$> counts)
            $ plot_bars_spacing .~ BarsFixGap 0 0
            $ plot_bars_alignment .~ BarsLeft
            $ plot_bars_item_styles .~ [(FillStyleSolid $ mkColor (opt^.hist_common.col) (opt^.hist_common.opacity), Just def)]
            $ def

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

-- Plot Histogram to GTK window
hist ∷ F.Foldable f ⇒ f Double → HistOption → IO ()
hist x opt = renderableToWindow (toRenderable $ hist_ x opt) (opt^.hist_common.width) (opt^.hist_common.height)

-- Plot Histogram to file
hist' ∷ F.Foldable f ⇒ f Double → HistOption → String → IO (PickFn ())
hist' xs opt = renderableToFile
    (fo_size .~ (opt^.hist_common.width, opt^.hist_common.height) $ def)
    (toRenderable $ hist_ xs opt)
