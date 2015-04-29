module Diagrams.Plots.Basic
    ( with
    , module Diagrams.Plots.Basic.Types

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
import Diagrams.Prelude hiding (width, height)
import Diagrams.Plots
import Diagrams.Plots.Basic.Types

barPlot :: PlotOpt Double Void BarOpt -> IO ()
barPlot opt = renderCairo (opt^.file) (dims2D w h) . barPlot' $ opt
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
linePlot opt = renderCairo (opt^.file) (dims2D w h) . linePlot' $ opt
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
heatmap opt = renderCairo (opt^.file) (dims2D w h) . heatmap' $ opt
  where
    w = opt^.width
    h = opt^.height

heatmap' :: PlotOpt [Double] Void HeatmapOpt -> DiaR2
heatmap' opt = plot
  where
    plot = padY (1+pady) $ padX (1+padx) $ center (showPlot $ area <+ (heat,BL))
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

    (padx,pady) = opt^.pads
{-# INLINE heatmap' #-}
