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

hist_ ∷ G.Vector v Double ⇒ v Double → HistOption → Layout Double Double
hist_ xs' opt = layout
    where
        layout = 
            layout_title .~ opt^.common^.title
            $ layout_plots .~ [plotBars bars]
            $ layout_x_axis . laxis_generate .~ (const xAxis)
            $ layout_x_axis . laxis_title .~ opt^.common.xlab
            $ layout_y_axis . laxis_title .~ "Frequency"
            $ layout_bottom_axis_visibility.axis_show_ticks .~ False
            $ def ∷ Layout Double Double
        bars = 
            plot_bars_values .~ (zip labels (return <$> counts))
            $ plot_bars_spacing .~ BarsFixGap 0 0
            $ plot_bars_alignment .~ BarsLeft
            $ plot_bars_item_styles .~ [(FillStyleSolid $ mkColor (opt^.common.col) (opt^.common.opacity), Just def)]
            $ def

        xs = G.toList xs'
        numBins = (opt^.breaks) xs
        labels = autoSteps numBins xs
        counts = V.toList $ histogram_ (length labels - 1) (minimum labels) (maximum labels) xs'

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
                    newLabels = let k = ceiling (fromIntegral (length labels) / 5)
                                    l = filter (\(i,_) → (i `mod` k) == 0) $ zip [1..] labels
                                 in snd $ unzip l
                    halfW = (labels!!1 - labels!!0) / 2
                    min' = minimum labels - halfW
                    max' = maximum labels + halfW

-- Plot Histogram to GTK window
hist ∷ G.Vector v Double ⇒ v Double → HistOption → IO ()
hist x opt = renderableToWindow (toRenderable $ hist_ x opt) (opt^.common.width) (opt^.common.height)

-- Plot Histogram to file
hist' ∷ G.Vector v Double ⇒ v Double → HistOption → String → IO (PickFn ())
hist' xs opt = renderableToFile
    (fo_size .~ (opt^.common.width, opt^.common.height) $ def)
    (toRenderable $ hist_ xs opt)
