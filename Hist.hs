{-# LANGUAGE OverloadedStrings, UnicodeSyntax, BangPatterns, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Hist (
    hist
    , hist'
    , common
    , breaks
    , freedmanDiaconis
    , squareRoot
    , riceRule
    )where

import Statistics.Sample.Histogram
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Gtk
import Control.Lens
import Data.Default.Class
import Statistics.Quantile
import Type
import Control.Applicative

type BreakRule = [Double] → Int

data HistOption = HistOption {
    _common ∷ PlotOption
    , _breaks ∷ BreakRule
    }

makeLenses ''HistOption

instance Default HistOption where
    def = HistOption {
        _common = ylab .~ "Frequency" $ def
        , _breaks = freedmanDiaconis
    }

--http://en.wikipedia.org/wiki/Freedman%E2%80%93Diaconis_rule
freedmanDiaconis ∷ BreakRule
freedmanDiaconis xs = round ((maximum xs - minimum xs) / binSzie)
    where
        binSzie = 2 * iqr xs * n**(-1/3)
        n = fromIntegral $ length xs
        iqr x = let x' = V.fromList x
                    quartile3 = weightedAvg 3 4 x' 
                    quartile1 = weightedAvg 1 4 x' 
                in quartile3 - quartile1

squareRoot ∷ BreakRule
squareRoot = round . sqrt . fromIntegral . length

riceRule ∷ BreakRule
riceRule xs = ceiling (2*(fromIntegral $ length xs)**(1/3))

hist_ ∷ G.Vector v Double ⇒ v Double → HistOption → Layout PlotIndex Double
hist_ xs' opt = layout
    where
        layout = 
            layout_title .~ opt^.common^.title
            $ layout_plots .~ [plotBars bars]
            $ layout_x_axis . laxis_generate .~ indexAxis
            $ layout_x_axis . laxis_title .~ opt^.common.xlab
            $ layout_y_axis . laxis_title .~ "Frequency"
            $ layout_left_axis_visibility.axis_show_ticks .~ False
            $ def ∷ Layout PlotIndex Double
        bars = 
            plot_bars_values .~ addIndexes (return <$> counts)
            $ plot_bars_spacing .~ BarsFixGap 0 0
            $ plot_bars_alignment .~ BarsLeft
            $ plot_bars_item_styles .~ (map (\ x → (FillStyleSolid $ mkColor x (opt^.common.opacity), Just def)) $ cycle (opt^.common.col))
            $ def

        xs = G.toList xs'
        numBins = (opt^.breaks) xs
        labels = autoSteps numBins xs
        counts = V.toList $ histogram_ (length labels - 1) (minimum labels) (maximum labels) xs'

        indexAxis :: Integral i => [i] -> AxisData i
        indexAxis vs = AxisData {
            _axis_visibility = def { _axis_show_ticks = False },
            _axis_viewport = vport,
            _axis_tropweiv = invport,
            _axis_ticks    = [],
            _axis_labels   = [zip [0..] $ fmap show labels],
            _axis_grid     = []
            }
          where
            vport r i = linMap id ( fromIntegral imin - 0.5
                                  , fromIntegral imax + 1.5) r (fromIntegral i)
            invport r z = invLinMap round fromIntegral (imin, imax) r z
            imin = minimum vs
            imax = maximum vs

-- Plot Histogram to GTK window
hist ∷ G.Vector v Double ⇒ v Double → HistOption → IO ()
hist x = swap renderableToWindow 480 480 . toRenderable . hist_ x
    where 
        swap = swap_2_3 . swap_1_2
        swap_1_2 = flip
        swap_2_3 = (.) flip

-- Plot Histogram to file
hist' ∷ G.Vector v Double ⇒ v Double → HistOption → String → IO (PickFn ())
hist' xs opt = renderableToFile def (toRenderable $ hist_ xs opt)
