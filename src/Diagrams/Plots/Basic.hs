{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Diagrams.Plots.Basic
    ( with
    -- * General plot options
    , PlotOpt
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

    -- * Line Options
    , showPoint 

    , barPlot
    , barPlot'
    , linePlot
    , linePlot'

    , Diagrams.Plots.Basic.heatmap
    , heatmap'
    ) where

import Control.Lens
import Data.Default
import Data.Monoid (mempty)
import Data.Void (Void)

import Diagrams.Backend.Cairo
import Diagrams.Prelude (SizeSpec2D(..), with, (===), rect, (<>), center)
import Diagrams.Plots

data PlotOpt datX datY o = PlotOpt
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
    , _plotOptExtra :: o
    }

makeFields ''PlotOpt

instance Default o => Default (PlotOpt datX datY o) where
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
        }

data LinePlotOpt = LinePlotOpt 
    { _linePlotOptShowPoint :: Bool
    }

makeFields ''LinePlotOpt

instance Default LinePlotOpt where
    def = LinePlotOpt
        { _linePlotOptShowPoint = False
        }

barPlot :: PlotOpt Double Void BarOpt -> IO ()
barPlot opt = renderCairo (opt^.file) (Dims w h) . barPlot' $ opt
  where
    w = opt^.width
    h = opt^.height

barPlot' :: PlotOpt Double Void BarOpt -> DiaR2
barPlot' opt = text' 0.3 (opt^.title) === plot
  where
    plot = showPlot $ area <+ (b, BL)
    area = plotArea (5.5*w/h) 5.5 (yAxis, def, def, xAxis)
    b = bars xs Nothing (opt^.extra)
    yAxis = indexAxis (length xs) (opt^.yNames) 0.2 def
    xAxis = realAxis (minimum xs, maximum xs) 0.2 def
    xs = opt^.x
    w = opt^.width
    h = opt^.height

linePlot :: PlotOpt Double Double LinePlotOpt -> IO ()
linePlot opt = renderCairo (opt^.file) (Dims w h) . linePlot' $ opt
  where
    w = opt^.width
    h = opt^.height

linePlot' :: PlotOpt Double Double LinePlotOpt -> DiaR2
linePlot' opt = text' 0.3 (opt^.title) === plot 
  where
    plot | opt^.extra.showPoint = showPlot $ area <+ (ps, BL) <+ (l, BL)
         | otherwise = showPlot $ area <+ (l, BL)
    area = plotArea (5.5*w/h) 5.5 (yAxis, def, def, xAxis)
    xAxis | null xs = indexAxis (length ys) (opt^.xNames) 0.2 def
          | otherwise = realAxis (minimum xs, maximum xs) 0.2 def
    yAxis = realAxis (minimum ys, maximum ys) 0.2 def
    l | null xs = line Nothing ys def
      | otherwise = line xs ys def
    ps | null xs = points Nothing ys def
       | otherwise = points xs ys def
    xs = opt^.x
    ys = opt^.y
    w = opt^.width
    h = opt^.height
{-# INLINE linePlot' #-}

heatmap :: PlotOpt [Double] Void HeatmapOpt -> IO ()
heatmap opt = renderCairo (opt^.file) (Dims w h) . heatmap' $ opt
  where
    w = opt^.width
    h = opt^.height

heatmap' :: PlotOpt [Double] Void HeatmapOpt -> DiaR2
heatmap' opt = text' 0.3 (opt^.title) === plot
  where
    plot = rect (w'+1) (h'+1) <> center (showPlot $ area <+ (heat,BL))
    area = plotArea w' h' (rmAxisDiag yAxis, def, def, rmAxisDiag xAxis)
    xAxis = indexAxis c (opt^.xNames) (w' / fromIntegral c / 2) $
        with & labelOpt .~ opt^.xLabelOpt
    yAxis = indexAxis r (reverse $ opt^.yNames) (h' / fromIntegral r / 2) $
        with & labelOpt .~ opt^.yLabelOpt
    heat = Diagrams.Plots.heatmap xs $ opt^.extra
    xs = opt^.x
    r = length xs
    c = length $ head xs
    w = opt^.width
    h = opt^.height
    w' = h' * w / h
    h' = 5.5
    rmAxisDiag (AxisFn a) = AxisFn $ fmap (\f -> axisDiag .~ mempty $ f) a
{-# INLINE heatmap' #-}
