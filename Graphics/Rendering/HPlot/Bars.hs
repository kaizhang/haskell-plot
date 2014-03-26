{-# LANGUAGE OverloadedStrings, UnicodeSyntax, BangPatterns, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Rendering.HPlot.Bars where

import qualified Data.Foldable as F
import Graphics.Rendering.Chart
import Control.Lens
import Data.Default
import Graphics.Rendering.HPlot.Types
import Data.List
import Data.Maybe

data BarOption = BarOption {
    _b_cols ∷ [String]
    , _b_opacity ∷ Double
    , _b_align ∷ PlotBarsAlignment
    , _b_space ∷ Double
    , _b_style ∷ PlotBarsStyle
    , _b_legend ∷ [String]
    }

makeLenses ''BarOption

instance Default BarOption where
    def = BarOption {
        _b_opacity = 1.0
        , _b_cols = ["blue", "red", "green", "yellow", "cyan", "magenta"]
        , _b_align = BarsCentered
        , _b_space = 15
        , _b_style = BarsClustered
        , _b_legend = []
    }

bars ∷ F.Foldable f ⇒ BarOption → (Maybe (f Double), [f Double]) → EitherPlot
bars opt (x, ys) | isNothing x = Left $ toPlot' $ addIndexes ys'
                 | otherwise = Right $ toPlot' $ zip (F.toList $ fromJust x) ys'
    where
        toPlot' x_y = plotBars $ plot_bars_values .~ x_y
            $ plot_bars_style .~ opt^.b_style
            $ plot_bars_titles .~ opt^.b_legend
            $ plot_bars_item_styles .~ fmap (\c → (FillStyleSolid $ mkColor c (opt^.b_opacity), Just def)) (cycle (opt^.b_cols))
            $ plot_bars_spacing .~ BarsFixGap (opt^.b_space) 0
            $ plot_bars_alignment .~ opt^.b_align
            $ def
        ys' = transpose $ fmap F.toList ys
