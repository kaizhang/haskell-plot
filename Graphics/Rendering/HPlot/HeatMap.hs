{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Graphics.Rendering.HPlot.HeatMap (
      heatmap
    , heatmap'
) where

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.HPlot.HeatMap.Internal
import Graphics.Rendering.HPlot.Types
import Data.Default
import Control.Lens

heatmap_ ∷ HeatMapOption → [[Double]] → Layout Double Double
heatmap_ opt xs = toLayout p
    where
        p = plotHeatMap 
            $ heat_map_values .~ xs
            $ heat_map_palette .~ opt^.palette
            $ heat_map_opacity .~ opt^.opacity
            $ heat_map_space .~ opt^.space
            $ def

        toLayout p' = 
              layout_plots .~ [p']
            $ layout_title .~ opt^.title
            $ layout_x_axis .~ (
                    laxis_generate .~ axisFn (opt^.labCol)
                  $ def
              )
            $ layout_y_axis .~ (
                    laxis_generate .~ axisFn (opt^.labRow)
                  $ def
              )
            $ def

        axisFn labs ps = mkHeatMapAxis labs ps' (l, u)
            where
                ps' = drop 2 ps
                [l, u] = take 2 ps

heatmap ∷ HeatMapOption → [[Double]] → IO ()
heatmap opt xs = renderableToWindow (toRenderable layout) (opt^.width) (opt^.height)
    where
        layout = heatmap_ opt xs

heatmap' ∷ HeatMapOption → [[Double]] → String → IO ()
heatmap' opt xs flname = 
    renderableToFile (fo_size .~ (opt^.width, opt^.height) $ def)
    (toRenderable layout) flname >> return ()
        where
            layout = heatmap_ opt xs

mkHeatMapAxis ∷ [String] → [Double] → (Double, Double) → AxisData Double
mkHeatMapAxis labs labelvs range = AxisData {
    _axis_visibility = def,
    _axis_viewport = vmap range,
    _axis_tropweiv = invmap range,
    _axis_ticks = [ (v, -5) | v ← labelvs ],
    _axis_grid = [],
    _axis_labels = [zip labelvs labs]
}
