{-# LANGUAGE TemplateHaskell #-}

module Diagrams.Plots.PlotArea 
    ( P(..)
    , PlotArea
    , plotAreaWidth
    , plotAreaHeight
    , plotAreaLeft
    , plotAreaRight
    , plotAreaTop
    , plotAreaBottom
    , plotAreaBackground
    , plotArea
    , showPlot
    , placeOn
    , (<+)
    ) where

import Diagrams.Prelude hiding (rotation)
import Control.Lens (makeLenses, (^.))

import Diagrams.Plots.Axis
import Diagrams.Plots.Types
import Diagrams.Plots.Utils (text')

-- | how to align a plot to the plot area
data P = BL  -- Bottom Left
       | TL  -- Top Left
       | TR  -- Top Right
       | BR  -- Bottom Right

data PlotArea = PlotArea
    { _plotAreaWidth :: !Double
    , _plotAreaHeight :: !Double
    , _plotAreaPlots :: ![DiaR2]
    , _plotAreaLeft :: !Axis -- ^ point map for left axis
    , _plotAreaTop :: !Axis -- ^ point map for top axis
    , _plotAreaRight :: !Axis -- ^ point map for right axis
    , _plotAreaBottom :: !Axis -- ^ point map for bottom axis
    , _plotAreaBackground :: !DiaR2
    }

makeLenses ''PlotArea

-- | construct a plot area
plotArea :: Double  -- ^ width
         -> Double  -- ^ height
         -> (AxisFn, AxisFn, AxisFn, AxisFn) -- ^ axes: left, top, right, bottom
         -> PlotArea
plotArea w h (l, t, r, b) = PlotArea w h [] lAxis tAxis rAxis bAxis background
  where
    lAxis = makeAxis l h
    tAxis = makeAxis t w
    rAxis = makeAxis r h
    bAxis = makeAxis b w
    background = lwL 0 $ moveTo ((w/2) ^& (h/2)) $ rect w h

showPlot :: PlotArea -> DiaR2
showPlot (PlotArea w h ps l t r b bgr) = mconcat 
    [ drawAxis 'l' l
    , translateY h . drawAxis 't' $ t
    , translateX w . drawAxis 'r' $ r
    , drawAxis 'b' b
    , mconcat ps
    , bgr
    ]

drawAxis :: Char -> Axis -> DiaR2
drawAxis p a
    | p == 'l' = (reflectX . rotateBy (1/4)) axis'
              <> mconcat ( flip map labels $ \((x,y), label) -> 
                    alignedText 1 0.5 label # rotateBy r
                                            # fontSizeO fontsize
                                            # font fontfamily
                                            # moveTo ((y+dx) ^& (x+dy)) )
    | p == 't' = reflectY axis'
              <> mconcat ( flip map labels $ \((x,y), label) ->
                    text' fontsize label # rotateBy r
                                         # moveTo ((x+dx) ^& (-y-dy)) )
    | p == 'r' = rotateBy (1/4) axis'
              <> mconcat ( flip map labels $ \((x,y), label) ->
                    alignedText 0 0.5 label # rotateBy r
                                            # fontSizeO fontsize
                                            # font fontfamily
                                            # moveTo ((-y-dx) ^& (x+dy)) )
    | p == 'b' = axis'
              <> mconcat ( flip map labels $ \((x,y), label) -> 
                    let t | r == 0 = text label
                          | otherwise = alignedText 1 0.5 label
                    in t # rotateBy r
                         # fontSizeO fontsize
                         # font fontfamily
                         # moveTo ((x+dx) ^& (y+dy)) )
    | otherwise = undefined
  where
    axis' = a^.axisDiag
    labels = a^.axisLabels
    dx = a^.axisLabelOpt^.offsetX
    dy = a^.axisLabelOpt^.offsetY
    fontsize = a^.axisLabelOpt^.size
    fontfamily = a^.axisLabelOpt^.fontFamily
    r = a^.axisLabelOpt^.rotation
{-# INLINE drawAxis #-}

placeOn :: (PlotFn, P) -> PlotArea -> PlotArea
placeOn (pltFn, p) area = plotAreaPlots %~ (mconcat plt:) $ area
  where
    plt = case p of
        BL -> pltFn bMap lMap
        TL -> pltFn tMap lMap
        TR -> pltFn tMap rMap
        BR -> pltFn bMap rMap
    lMap = area^.plotAreaLeft^.axisMap
    bMap = area^.plotAreaBottom^.axisMap
    tMap = area^.plotAreaTop^.axisMap
    rMap = area^.plotAreaRight^.axisMap

(<+) :: PlotArea -> (PlotFn, P) -> PlotArea
infixl 1 <+
(<+) = flip placeOn
