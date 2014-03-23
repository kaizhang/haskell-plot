{-# LANGUAGE OverloadedStrings, UnicodeSyntax, BangPatterns, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Point where

import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Gtk
import Control.Lens
import Data.Default.Class
import Type

data PointOption = PointOption {
    _common ∷ PlotOption
    , _radius ∷ Double
    }

makeLenses ''PointOption

instance Default PointOption where
    def = PointOption {
        _common = def
        , _radius = 2.5
    }

points_ ∷ G.Vector v Double ⇒ (Maybe (v Double), v Double) → PointOption → Layout Double Double
points_ (x,y) opt = layout
    where
        layout = 
            layout_title .~ opt^.common^.title
            $ layout_plots .~ [toPlot ps]
            $ layout_x_axis . laxis_title .~ opt^.common.xlab
            $ layout_y_axis . laxis_title .~ opt^.common.ylab
            $ def ∷ Layout Double Double
        ps = 
            plot_points_values .~ x_y
            $ plot_points_style .~ (
                point_radius .~ opt^.radius
                $ point_color .~ (mkColor (opt^.common.col) (opt^.common.opacity))
                $ def)
            $ def
        x_y = case x of
            Nothing → zip [1..] $ G.toList y
            Just x' → zip (G.toList x') $ G.toList y


points ∷ G.Vector v Double ⇒ (Maybe (v Double), v Double) → PointOption → IO ()
points (x,y) opt = renderableToWindow (toRenderable $ points_ (x,y) opt) (opt^.common.width) (opt^.common.height)

points' ∷ G.Vector v Double ⇒ (Maybe (v Double), v Double) → PointOption → String → IO (PickFn ())
points' (x,y) opt = renderableToFile
    (fo_size .~ (opt^.common.width, opt^.common.height) $ def)
    (toRenderable $ points_ (x,y) opt)
