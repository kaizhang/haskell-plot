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
    ) where

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Gtk
import Control.Lens
import Data.Default
import Data.Colour
import Data.Colour.Names
import Data.Maybe

data PlotOption = PlotOption {
    _title ∷ String
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
        , _xlab = []
        , _ylab = []
        , _xlim = (0, -1)
        , _width = 480
        , _height = 480
    }

mkColor ∷ String → Double → AlphaColour Double
mkColor c = withOpacity (fromMaybe blue $ readColourName c)

plot_ ∷ PlotOption → [Plot Double Double] → Layout Double Double
plot_ opt ps = 
    layout_title .~ opt^.title
        $ layout_plots .~ ps
        $ layout_x_axis .~ (
            laxis_title .~ opt^.xlab
            $ if uncurry (>=) (opt^.xlim)
                 then def
                 else laxis_generate .~ scaledAxis def (opt^.xlim) $ def)
             $ layout_y_axis . laxis_title .~ opt^.ylab
             $ def

plot ∷ PlotOption → [Plot Double Double] → IO ()
plot opt ps = renderableToWindow (toRenderable $ plot_ opt ps) (opt^.width) (opt^.height)

plot' ∷ PlotOption → [Plot Double Double] → String → IO (PickFn ())
plot' opt ps = renderableToFile 
    (fo_size .~ (opt^.width, opt^.height) $ def)
    (toRenderable $ plot_ opt ps)
