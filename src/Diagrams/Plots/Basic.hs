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

    -- * Line Options
    , showPoint 

    , linePlot
    , linePlot'

    , Diagrams.Plots.Basic.heatmap
    , heatmap'
    ) where

import Diagrams.Prelude (SizeSpec2D(..), with, (===))
import Diagrams.Backend.Cairo
import Data.Default
import Control.Lens
import Data.Void (Void)

import Diagrams.Plots

data PlotOpt datX datY o = PlotOpt
    { _plotX :: [datX]
    , _plotY :: [datY]
    , _plotHeight :: Double
    , _plotWidth :: Double
    , _plotXlab :: String
    , _plotYlab :: String
    , _plotXNames :: [String]
    , _plotYNames :: [String]
    , _plotTitle :: String
    , _plotFile :: String
    , _plotExtra :: o
    }

makeFields ''PlotOpt

instance Default o => Default (PlotOpt datX datY o) where
    def = PlotOpt
        { _plotX = []
        , _plotY = []
        , _plotHeight = 480
        , _plotWidth = 480
        , _plotXlab = ""
        , _plotYlab = ""
        , _plotXNames = []
        , _plotYNames = []
        , _plotTitle = ""
        , _plotFile = "plot.png"
        , _plotExtra = def
        }

data LinePlotOpt = LinePlotOpt 
    { _lineplotShowPoint :: Bool
    }

makeFields ''LinePlotOpt

instance Default LinePlotOpt where
    def = LinePlotOpt
        { _lineplotShowPoint = False
        }

linePlot :: PlotOpt Double Double LinePlotOpt -> IO ()
linePlot opt = renderCairo (opt^.file) (Dims w h) . linePlot' $ opt
  where
    w = opt^.width
    h = opt^.height

linePlot' :: PlotOpt Double Double LinePlotOpt -> DiaR2
linePlot' opt = text' (opt^.title) === plot 
  where
    plot | opt^.extra.showPoint =  showPlot $ area <+ (ps, BL) <+ (l, BL)
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
heatmap' opt = text' (opt^.title) === plot
  where
    plot = showPlot $ area <+ (heat,BL)
    area = plotArea (5.5*w/h) 5.5 (yAxis, def, def, xAxis)
    xAxis = indexAxis c (opt^.xNames) (w' / fromIntegral c / 2) $
        with & tickLen .~ (-0.05)
             & labelOpt.rotation .~ (1/4)
    yAxis = indexAxis r (reverse $ opt^.yNames) (h' / fromIntegral r / 2) $ with & tickLen .~ (-0.05)
    heat = Diagrams.Plots.heatmap xs def
    xs = opt^.x
    r = length xs
    c = length $ head xs
    w = opt^.width
    h = opt^.height
    w' = h' * w / h
    h' = 5.5
{-# INLINE heatmap' #-}
