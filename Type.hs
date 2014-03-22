{-# LANGUAGE OverloadedStrings, UnicodeSyntax, TemplateHaskell, BangPatterns #-}

module Type (
    PlotOption
    , title
    , xlab
    , ylab
    , opacity
    , col
    , mkColor
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
    , _opacity ∷ Double
    , _col ∷ [String]
    } deriving (Show)

makeLenses ''PlotOption

instance Default PlotOption where
    def = PlotOption {
        _title = []
        , _xlab = []
        , _ylab = []
        , _opacity = 1.0
        , _col = ["blue", "red", "green", "yellow", "cyan", "magenta"]
    }

mkColor ∷ String → Double → AlphaColour Double
mkColor c = withOpacity (fromMaybe blue $ readColourName c)
