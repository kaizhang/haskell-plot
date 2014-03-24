{-# LANGUAGE OverloadedStrings, UnicodeSyntax, BangPatterns, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Rendering.HPlot.Points where

import qualified Data.Foldable as F
import Graphics.Rendering.Chart
import Control.Lens
import Data.Default
import Graphics.Rendering.HPlot.Types

data PointOption = PointOption {
    _p_radius ∷ Double
    , _p_shape ∷ Char
    , _p_col ∷ String
    , _p_opacity ∷ Double
    , _p_thickness ∷ Double
    }

makeLenses ''PointOption

instance Default PointOption where
    def = PointOption {
        _p_radius = 3
        , _p_shape = '.'
        , _p_thickness = 1
        , _p_opacity = 1.0
        , _p_col = "blue"
    }

toPointStyle ∷ PointOption → PointStyle
toPointStyle opt = case () of
    _ | s `elem` ".●" → filledCircles r color
      | s `elem` "o○" → hollowCircles r thick color
      | s `elem` "x" → exes r thick color
      | s `elem` "+" → plusses r thick color
      | s `elem` "*" → stars r thick color
      | s `elem` "v▼" → filledPolygon r 3 True color
      | s `elem` "^▲" → filledPolygon r 3 False color
      | s `elem` "#■" → filledPolygon r 4 False color
      | s `elem` "◆" → filledPolygon r 4 True color
      | otherwise → filledCircles r color
        where
            s = opt^.p_shape
            r = opt^.p_radius
            color = mkColor (opt^.p_col) (opt^.p_opacity)
            thick = opt^.p_thickness

points ∷ F.Foldable f ⇒ PointOption → (Maybe (f Double), f Double) → Plot Double Double
points opt (x,y) = toPlot $ plot_points_values .~ x_y
    $ plot_points_style .~ toPointStyle opt
    $ def
        where
            x_y = case x of
                Nothing → zip [1..] $ F.toList y
                Just x' → zip (F.toList x') $ F.toList y
