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
import Control.Lens
import Data.Default
import Data.Either

plot_ ∷ PlotOption → [EitherPlot] → EitherLayout
plot_ opt ps | null ls = Right $ toLayout rs axisFnRight
             | otherwise = Left $ toLayout ls axisFnLeft
    where
        toLayout x axisFn = layout_title .~ opt^.title
            $ layout_plots .~ x
            $ layout_x_axis .~ (
                laxis_title .~ opt^.xlab
                $ laxis_generate .~ axisFn $ def)
            $ layout_y_axis . laxis_title .~ opt^.ylab
            $ def

        (ls, rs) = partitionEithers ps
        axisFnLeft = autoIndexAxis (opt^.labels)
        axisFnRight = if uncurry (>=) (opt^.xlim)
                         then autoAxis
                         else scaledAxis def (opt^.xlim)

plot ∷ PlotOption → [EitherPlot] → IO ()
plot opt ps = either f f (plot_ opt ps)
    where f x = renderableToWindow (toRenderable x) (opt^.width) (opt^.height)

plot' ∷ PlotOption → [EitherPlot] → String → IO (PickFn ())
plot' opt ps = either f f (plot_ opt ps)
    where f x = renderableToFile (fo_size .~ (opt^.width, opt^.height) $ def) (toRenderable x)
