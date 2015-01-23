{-# LANGUAGE TemplateHaskell #-}

module Diagrams.Plots.Simple
    ( x
    , y
    , height
    , width
    , title
    , file
    , showPoint 
    , labels
    , linePlot
    , with
    , linePlot'
    ) where

import Diagrams.Prelude (SizeSpec2D(..), with, (===))
import Diagrams.Backend.Cairo
import Data.Default
import Control.Lens

import Diagrams.Plots

data LinePlotOpt = LinePlotOpt 
    { _x :: [Double]
    , _y :: [Double]
    , _height :: Double
    , _width :: Double
    , _labels :: [String]
    , _title :: String
    , _file :: String
    , _showPoint :: Bool
    }

makeLenses ''LinePlotOpt

instance Default LinePlotOpt where
    def = LinePlotOpt
        { _x = []
        , _y = []
        , _height = 480
        , _width = 480
        , _labels = []
        , _title = ""
        , _file = "plot.svg"
        , _showPoint = False
        }

linePlot :: LinePlotOpt -> IO ()
linePlot opt = renderCairo (_file opt) (Dims w h) . linePlot' $ opt
  where
    w = _width opt
    h = _height opt

linePlot' :: LinePlotOpt -> DiaR2
linePlot' opt = text' (_title opt) === plot 
  where
    plot | _showPoint opt =  showPlot $ area <+ (ps, BL) <+ (l, BL)
         | otherwise = showPlot $ area <+ (l, BL)
    area = plotArea (5.5*w/h) 5.5 (yAxis, def, def, xAxis)
    xAxis | null xs = indexAxis (length ys) (_labels opt) 0.2 def
          | otherwise = realAxis (minimum xs, maximum xs) 0.2 def
    yAxis = realAxis (minimum ys, maximum ys) 0.2 def
    l | null xs = line Nothing ys def
      | otherwise = line xs ys def
    ps | null xs = points Nothing ys def
       | otherwise = points xs ys def
    xs = _x opt
    ys = _y opt
    w = _width opt
    h = _height opt
{-# INLINE linePlot' #-}
