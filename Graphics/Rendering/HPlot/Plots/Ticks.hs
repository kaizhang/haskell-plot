{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.HPlot.Plots.Ticks
    ( ticks
    ) where

import Diagrams.Prelude
import Data.Default
import Control.Lens hiding ((#))
import Data.Maybe
import Graphics.Rendering.HPlot.Types

data TickOpts = TickOpts
    { _tickLength :: Double
    }

instance Default TickOpts where
    def = TickOpts 
        { _tickLength = 0.1
        }

makeLenses ''TickOpts

ticks :: (PlotData m1 a1, PlotData m2 a2) => m1 a1 -> m2 a2 -> TickOpts -> DelayPlot
ticks xs ys opt (mapX, mapY) = [ticksX, ticksY]
  where
    ticksX = mconcat [ fromVertices [x ^& 0, x ^& (opt^.tickLength)] | x <- mapMaybe (runMap mapX) xs' ]
    ticksY = mconcat [ fromVertices [0 ^& y, (opt^.tickLength) ^& y] | y <- mapMaybe (runMap mapY) ys' ]
    xs' = getValues xs
    ys' = getValues ys
