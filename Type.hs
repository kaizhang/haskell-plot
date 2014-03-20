{-# LANGUAGE OverloadedStrings, UnicodeSyntax, TemplateHaskell, BangPatterns #-}

module Type (
    PlotOption
    , title
    , xlab
    , ylab
    ) where

import Control.Lens
import Data.Default

data PlotOption = PlotOption {
    _title ∷ String
    , _xlab ∷ String
    , _ylab ∷ String
    } deriving (Show)

makeLenses ''PlotOption

instance Default PlotOption where
    def = PlotOption {
        _title = [],
        _xlab = [],
        _ylab = []
    }
