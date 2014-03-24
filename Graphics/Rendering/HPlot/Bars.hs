{-# LANGUAGE OverloadedStrings, UnicodeSyntax, BangPatterns, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Rendering.HPlot.Bars where

import qualified Data.Foldable as F
import Graphics.Rendering.Chart
import Control.Lens
import Data.Default
import Graphics.Rendering.HPlot.Types
import Data.List

data BarOption = BarOption {
    _col ∷ [String]
    , _opacity ∷ Double
    , _thickness ∷ Double
    , _align ∷ PlotBarsAlignment
    , _space ∷ Double
    }

makeLenses ''BarOption

instance Default BarOption where
    def = BarOption {
        _thickness = 1
        , _opacity = 1.0
        , _col = ["blue"]
        , _align = BarsCentered
        , _space = 10
    }

bars ∷ F.Foldable f ⇒ BarOption → (Maybe (f Double), [f Double]) → Plot Double Double
bars opt (x,ys) = plotBars $ plot_bars_values .~ x_y
    $ plot_bars_item_styles .~ fmap (\c → (FillStyleSolid $ mkColor c (opt^.opacity), Just def)) (cycle (opt^.col))
    $ plot_bars_spacing .~ BarsFixGap (opt^.space) 0
    $ plot_bars_alignment .~ opt^.align
    $ def
        where
            ys' = transpose $ fmap F.toList ys
            x_y = case x of
                Nothing → zip [1..] ys'
                Just x' → zip (F.toList x') ys'
