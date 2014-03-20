{-# LANGUAGE OverloadedStrings, UnicodeSyntax, BangPatterns, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Hist (
    hist
    , common
    , breaks
    )where

import Statistics.Sample.Histogram
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Colour
import Data.Colour.Names
import Control.Lens
import Data.Default.Class
import Statistics.Quantile
import Type

data HistOption = HistOption {
    _common ∷ PlotOption
    , _breaks ∷ [Double] → Int
    }

makeLenses ''HistOption

instance Default HistOption where
    def = HistOption {
        _common = ylab .~ "Frequency" $ def
        , _breaks = freedman_diaconis
    }

--http://en.wikipedia.org/wiki/Freedman%E2%80%93Diaconis_rule
freedman_diaconis ∷ [Double] → Int
freedman_diaconis xs = round ((maximum xs - minimum xs) / binSzie)
    where
        binSzie = 2 * iqr xs * n**(-1/3)
        n = fromIntegral $ length xs
        iqr x = let x' = V.fromList x
                    quartile3 = weightedAvg 3 4 x' 
                    quartile1 = weightedAvg 1 4 x' 
                in quartile3 - quartile1

hist ∷ G.Vector v Double ⇒ v Double → HistOption → String → IO (PickFn ())
hist xs' opt = renderableToFile def (toRenderable layout)
    where
        layout = 
            layout_title .~ opt^.common^.title
            $ layout_plots .~ [plotBars bars]
            $ layout_x_axis . laxis_generate .~ autoIndexAxis (fmap show labels)
            $ layout_x_axis . laxis_title .~ opt^.common.xlab
            $ layout_y_axis . laxis_title .~ opt^.common.ylab
            $ def ∷ Layout PlotIndex Double
        bars = 
            plot_bars_values .~ addIndexes (fmap return $ counts)
            $ plot_bars_spacing .~ BarsFixGap 0 0
            $ plot_bars_alignment .~ BarsLeft
            $ def

        xs = G.toList xs'
        numBins = (opt^.breaks) xs
        labels = autoSteps numBins xs
        counts = V.toList $ histogram_ (length labels) (minimum labels) (maximum labels) xs'
