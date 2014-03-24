{-# LANGUAGE OverloadedStrings, UnicodeSyntax, TemplateHaskell, BangPatterns #-}

module Graphics.Rendering.HPlot.Type (
    PlotOption
    , title
    , xlab
    , ylab
    , xlim
    , opacity
    , col
    , mkColor
    , width
    , height
    ) where

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
    , _opacity ∷ Double
    , _col ∷ String
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
        , _opacity = 1.0
        , _col = "blue"
        , _width = 480
        , _height = 480
    }

mkColor ∷ String → Double → AlphaColour Double
mkColor c = withOpacity (fromMaybe blue $ readColourName c)
