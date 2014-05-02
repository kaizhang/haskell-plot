{-# LANGUAGE OverloadedStrings, UnicodeSyntax, BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.HPlot.Plot where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Control.Lens (makeLenses, (^.), mapped, _1, _2)

import Graphics.Rendering.HPlot.Types
import Graphics.Rendering.HPlot.Axis

data P = BL
       | TL
       | TR
       | BR

data PlotArea = PlotArea
    { _plotAreaWidth ∷ Double
    , _plotAreaHeight ∷ Double
    , _plotAreaPlots ∷ [Diagram B R2]
    , _plotAreaLeft ∷ Axis -- ^ point map for left axis
    , _plotAreaTop ∷ Axis -- ^ point map for top axis
    , _plotAreaRight ∷ Axis -- ^ point map for right axis
    , _plotAreaBottom ∷ Axis -- ^ point map for bottom axis
    , _plotAreaBackground ∷ Diagram B R2
    }

makeLenses ''PlotArea

plotArea ∷ Double 
         → Double 
         → (AxisFn, AxisFn, AxisFn, AxisFn)
         → PlotArea
plotArea w h (l, t, r, b) = PlotArea w h [] lAxis tAxis rAxis bAxis background
  where
    lAxis = makeAxis l h
    tAxis = makeAxis t w
    rAxis = makeAxis r h
    bAxis = makeAxis b w
    background = lw 0 $ moveTo ((w/2) ^& (h/2)) $ rect w h

showPlot ∷ PlotArea → Diagram B R2
showPlot (PlotArea w h ps l t r b bgr) = mconcat [lAxisD, translateY h tAxisD, translateX w rAxisD, bAxisD, plts, bgr]
  where
    plts = mconcat ps
    f (xy, label) = (p2 xy, text' label)
    lAxisD = axis' <> labels
      where
        axis' = reflectX.rotateBy (1/4) $ l^.axisDiag
        labels = mconcat [ text' label # alignR # moveTo (y ^& x) | ((x, y), label) ← l^.axisLabels ]
    tAxisD = reflectY (t^.axisDiag) <> labels
      where
        labels = position $ map f $ mapped._1._2 %~ (*(-1)) $ t^.axisLabels
    rAxisD = axis' <> labels
      where
        axis' = rotateBy (1/4) $ r^.axisDiag
        labels = mconcat [ text' label # alignL # moveTo ((-y) ^& x) | ((x, y), label) ← r^.axisLabels ]
    bAxisD = (b^.axisDiag) <> labels
      where
        labels = position $ map f $ b^.axisLabels

placeOn ∷ DelayPlot → PlotArea → P → PlotArea
placeOn pltFn area p = plotAreaPlots %~ (mconcat plt:) $ area
  where
    plt = case p of
        BL → pltFn (bMap, lMap)
        TL → pltFn (tMap, lMap)
        TR → pltFn (tMap, rMap)
        BR → pltFn (bMap, rMap)
    lMap = area^.plotAreaLeft^.axisMap
    bMap = area^.plotAreaBottom^.axisMap
    tMap = area^.plotAreaTop^.axisMap
    rMap = area^.plotAreaRight^.axisMap

concatP ∷ [(PointMap Double, PointMap Double) → Diagram B R2] → (PointMap Double, PointMap Double) → Diagram B R2
concatP ps pMap = mconcat $ sequence ps pMap

minMax ∷ [Double] → (Double, Double)
minMax x = (minimum x, maximum x)

