{-# LANGUAGE OverloadedStrings, UnicodeSyntax, BangPatterns, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Rendering.HPlot.Lines where

import qualified Data.Foldable as F
import Graphics.Rendering.Chart
import Control.Lens
import Data.Default
import Graphics.Rendering.HPlot.Types

data LineOption = LineOption {
    _l_width ∷ Double
    , _l_col ∷ String
    , _l_opacity ∷ Double
    , _l_type ∷ Int
    , _l_join ∷ LineJoin
    }

makeLenses ''LineOption

instance Default LineOption where
    def = LineOption {
        _l_width = 1
        , _l_opacity = 1.0
        , _l_col = "blue"
        , _l_type = 1
        , _l_join = LineJoinMiter
    }

toLineStyle ∷ LineOption → LineStyle
toLineStyle opt = line_width .~ opt^.l_width
    $ line_color .~ mkColor (opt^.l_col) (opt^.l_opacity)
    $ line_join .~ opt^.l_join
    $ line_dashes .~ (case opt^.l_type of
        1 → []
        2 → [5,5]
        3 → [2,4]
        4 → [5,3,2,3]
        5 → [8,3]
        6 → [8,3,3,3]
        _ → [])
    $ def

line ∷ F.Foldable f ⇒ LineOption → (Maybe (f Double), f Double) → Plot Double Double
line opt (x,y) = toPlot $ plot_lines_values .~ [x_y]
    $ plot_lines_style .~ toLineStyle opt
    $ def
        where
            x_y = case x of
                Nothing → zip [1..] $ F.toList y
                Just x' → zip (F.toList x') $ F.toList y
