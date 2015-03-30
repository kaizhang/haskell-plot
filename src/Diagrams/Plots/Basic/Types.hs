{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Diagrams.Plots.Basic.Types
    ( 
    -- * General plot options
    PlotOpt
    , x
    , y
    , height
    , width
    , title
    , file
    , xlab
    , ylab
    , xNames
    , yNames
    , xLabelOpt
    , yLabelOpt
    , extra
    , pads

    -- * Line Options
    , LinePlotOpt
    , showPoint 

    ) where

import Control.Lens
import Data.Default

import Diagrams.Plots

data PlotOpt datX datY opt = PlotOpt
    { _plotOptX :: [datX]
    , _plotOptY :: [datY]
    , _plotOptHeight :: Double
    , _plotOptWidth :: Double
    , _plotOptXlab :: String
    , _plotOptYlab :: String
    , _plotOptXNames :: [String]
    , _plotOptYNames :: [String]
    , _plotOptXLabelOpt :: LabelOpt
    , _plotOptYLabelOpt :: LabelOpt
    , _plotOptTitle :: String
    , _plotOptFile :: String
    , _plotOptExtra :: opt
    , _plotOptPads :: (Double, Double)  -- (x,y)
    }

makeFields ''PlotOpt

instance Default opt => Default (PlotOpt datX datY opt) where
    def = PlotOpt
        { _plotOptX = []
        , _plotOptY = []
        , _plotOptHeight = 480
        , _plotOptWidth = 480
        , _plotOptXlab = ""
        , _plotOptYlab = ""
        , _plotOptXNames = []
        , _plotOptYNames = []
        , _plotOptXLabelOpt = def
        , _plotOptYLabelOpt = def
        , _plotOptTitle = ""
        , _plotOptFile = "plot.png"
        , _plotOptExtra = def
        , _plotOptPads = (0.1,0.1)
        }

data LinePlotOpt = LinePlotOpt 
    { _linePlotOptShowPoint :: Bool
    }

makeFields ''LinePlotOpt

instance Default LinePlotOpt where
    def = LinePlotOpt
        { _linePlotOptShowPoint = False
        }
