{-# LANGUAGE OverloadedStrings, UnicodeSyntax, BangPatterns, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Rendering.HPlot.Point where

import qualified Data.Foldable as F
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Gtk
import Control.Lens
import Data.Default
import Graphics.Rendering.HPlot.Type

data PointOption = PointOption {
    _common ∷ PlotOption
    , _radius ∷ Double
    , _shape ∷ Char
    , _thickness ∷ Double
    }

makeLenses ''PointOption

instance Default PointOption where
    def = PointOption {
        _common = def
        , _radius = 3
        , _shape = '.'
        , _thickness = 1
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
            s = opt^.shape
            r = opt^.radius
            color = mkColor (opt^.common.col) (opt^.common.opacity)
            thick = opt^.thickness

points_ ∷ F.Foldable f ⇒ (Maybe (f Double), f Double) → PointOption → Layout Double Double
points_ (x,y) opt = layout
    where
        layout = 
            layout_title .~ opt^.common^.title
            $ layout_plots .~ [toPlot ps]
            $ layout_x_axis .~ (
                laxis_title .~ opt^.common.xlab
                $ if uncurry (>=) (opt^.common.xlim)
                     then def
                     else laxis_generate .~ scaledAxis def (opt^.common.xlim) $ def)
            $ layout_y_axis . laxis_title .~ opt^.common.ylab
            $ def ∷ Layout Double Double
        ps = 
            plot_points_values .~ x_y
            $ plot_points_style .~ toPointStyle opt
            $ def
        x_y = case x of
            Nothing → zip [1..] $ F.toList y
            Just x' → zip (F.toList x') $ F.toList y


points ∷ F.Foldable f ⇒ (Maybe (f Double), f Double) → PointOption → IO ()
points (x,y) opt = renderableToWindow (toRenderable $ points_ (x,y) opt) (opt^.common.width) (opt^.common.height)

points' ∷ F.Foldable f ⇒ (Maybe (f Double), f Double) → PointOption → String → IO (PickFn ())
points' (x,y) opt = renderableToFile
    (fo_size .~ (opt^.common.width, opt^.common.height) $ def)
    (toRenderable $ points_ (x,y) opt)
