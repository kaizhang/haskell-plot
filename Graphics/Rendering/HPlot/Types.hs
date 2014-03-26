{-# LANGUAGE OverloadedStrings, UnicodeSyntax, TemplateHaskell, BangPatterns #-}

module Graphics.Rendering.HPlot.Types (
    PlotOption(..)
    , title
    , xlab
    , ylab
    , xlim
    , mkColor
    , width
    , height
    , plot_
    , plot
    , plot'
    , EitherPlot
    , EitherLayout
    , labels
    ) where

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Gtk
import Control.Lens
import Data.Default
import Data.Colour
import Data.Colour.Names
import Data.Either
import Data.Maybe

data PlotOption = PlotOption {
    _title ∷ String
    , _labels ∷ [String]
    , _xlab ∷ String
    , _ylab ∷ String
    , _xlim ∷ (Double, Double)
    , _width ∷ Int
    , _height ∷ Int
    } deriving (Show)

makeLenses ''PlotOption

instance Default PlotOption where
    def = PlotOption {
        _title = []
        , _labels = []
        , _xlab = []
        , _ylab = []
        , _xlim = (0, -1)
        , _width = 480
        , _height = 480
    }

type EitherPlot = Either (Plot PlotIndex Double) (Plot Double Double)
type EitherLayout = Either (Layout PlotIndex Double) (Layout Double Double)

mkColor ∷ String → Double → AlphaColour Double
mkColor c = withOpacity (fromMaybe blue $ readColourName c)

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
