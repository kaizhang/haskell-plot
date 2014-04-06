{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Graphics.Rendering.HPlot.Bars where

import qualified Data.Foldable as F
import Graphics.Rendering.Chart
import Control.Lens
import Data.Default
import Graphics.Rendering.HPlot.Types
import Graphics.Rendering.HPlot.Utils
import Data.List
import Data.Maybe

bars ∷ F.Foldable f ⇒ (Maybe (f Double), [f Double]) → BarOption → EitherPlot
bars (x, ys) opt | isNothing x = Left $ toPlot' $ addIndexes ys'
                 | otherwise = Right $ toPlot' $ zip (F.toList $ fromJust x) ys'
    where
        toPlot' x_y = plotBars $ plot_bars_values .~ x_y
            $ plot_bars_style .~ opt^.style
            $ plot_bars_titles .~ opt^.legend
            $ plot_bars_item_styles .~ fmap (\c → (FillStyleSolid $ mkColor c (opt^.opacity), Just def)) (cycle (opt^.cols))
            $ plot_bars_spacing .~ BarsFixGap (opt^.space) 0
            $ plot_bars_alignment .~ opt^.align
            $ def
        ys' = transpose $ fmap F.toList ys
