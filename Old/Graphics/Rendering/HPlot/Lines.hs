{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Rendering.HPlot.Lines where

import qualified Data.Foldable as F
import Graphics.Rendering.Chart
import Control.Lens
import Data.Default
import Graphics.Rendering.HPlot.Types
import Graphics.Rendering.HPlot.Utils
import Data.Maybe

toLineStyle ∷ LineOption → LineStyle
toLineStyle opt = line_width .~ opt^.lwd
    $ line_color .~ mkColor (opt^.col) (opt^.opacity)
    $ line_dashes .~ (case opt^.lty of
        1 → []
        2 → [5,5]
        3 → [2,4]
        4 → [5,3,2,3]
        5 → [8,3]
        6 → [8,3,3,3]
        _ → [])
    $ def

line ∷ F.Foldable f ⇒ (Maybe (f Double), f Double) → LineOption → EitherPlot
line (x,y) opt | isNothing x = Left $ mkPlot $ addIndexes y'
               | otherwise = Right $ mkPlot $ zip (F.toList $ fromJust x) y'
    where
        y' = F.toList y
        mkPlot x_y = toPlot $ plot_lines_values .~ [x_y]
            $ plot_lines_style .~ toLineStyle opt
            $ def
