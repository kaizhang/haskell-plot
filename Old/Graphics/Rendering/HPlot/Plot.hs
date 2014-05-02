{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Graphics.Rendering.HPlot.Plot (
      plot_
    , plot
    , plot'
    ) where

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.HPlot.Types
import Graphics.Rendering.HPlot.Utils
import Control.Lens
import Data.Default
import Data.Either

plot_ ∷ [EitherPlot] → PlotOption → EitherLayout
{-# INLINE plot_ #-}
plot_ ps opt | null ls   = Right $ toLayout rs xAxisFnR
             | otherwise = Left $ toLayout ls xAxisFnL
    where
        toLayout x xAxisFn = layout_title .~ opt^.title
            $ layout_plots .~ x
            $ layout_x_axis .~ (
                  laxis_title .~ opt^.xlab
                $ laxis_generate .~ xAxisFn
                $ laxis_style .~ xAxisStyle
                $ def
            )
            $ layout_bottom_axis_visibility .~ xAxisVisibility
            $ layout_y_axis .~ (
                  laxis_title .~ opt^.ylab
                $ laxis_generate .~ yAxisFn
                $ laxis_style .~ yAxisStyle
                $ def
            )
            $ layout_left_axis_visibility .~ yAxisVisibility
            $ def

        (ls, rs) = partitionEithers ps
        xAxisFnL = autoIndexAxis (opt^.labels)
        xAxisFnR = if uncurry (>=) (opt^.xlim)
                      then autoAxis
                      else scaledAxis def (opt^.xlim)
        yAxisFn = if uncurry (>=) (opt^.ylim)
                     then autoAxis
                     else scaledAxis def (opt^.ylim)
        xAxisStyle = axis_grid_style .~ xGridStyle
            $ def
        yAxisStyle = axis_grid_style .~ yGridStyle
            $ def
        xGridStyle = case opt^.grid of
            'b' → defaultGridLineStyle
            'x' → defaultGridLineStyle
            _ → line_color .~ mkColor "black" 0 $ def
        yGridStyle = case opt^.grid of
            'b' → defaultGridLineStyle
            'y' → defaultGridLineStyle
            _ → line_color .~ mkColor "black" 0 $ def
        xAxisVisibility | opt^.axes == 0 = notVisible
                        | opt^.axes == 2 = notVisible
                        | otherwise = def
        yAxisVisibility | opt^.axes == 0 = notVisible
                        | opt^.axes == 1 = notVisible
                        | otherwise = def
        notVisible = AxisVisibility False False True

plot ∷ [EitherPlot] → PlotOption → IO ()
plot ps opt = either f f (plot_ ps opt)
    where f x = renderableToWindow (toRenderable x) (opt^.width) (opt^.height)

plot' ∷ [EitherPlot] → PlotOption → String → IO ()
plot' ps opt flname = either f f (plot_ ps opt) flname >> return ()
    where f x = renderableToFile (fo_size .~ (opt^.width, opt^.height) $ def) (toRenderable x)
