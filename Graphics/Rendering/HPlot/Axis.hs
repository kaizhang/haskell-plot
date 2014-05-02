{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module Graphics.Rendering.HPlot.Axis where

import Diagrams.Prelude hiding (pad)
import Diagrams.Backend.SVG
import Data.Default
import Control.Lens (makeLenses, (^.))
import Graphics.SVGFonts.ReadFont
import Graphics.Rendering.HPlot.Utils
import Graphics.Rendering.HPlot.Types

data LineStyle = LineStyle
    { _lineWidth ∷ Double
    , _lineColor ∷ AlphaColour Double
--    , _lineDashes ∷ [Double]
--    , _lineCap ∷ LineCap
--    , _lineJoin ∷ LineJoin
    }

withLineSty ∷ HasStyle a ⇒ LineStyle → a → a
withLineSty sty = lw (_lineWidth sty) . lcA (_lineColor sty)

data Axis = Axis
    { _axisMap ∷ PointMap Double
    , _axisLabels ∷ [((Double, Double), String)]
    , _axisDiag ∷ Diagram B R2
    }

makeLenses ''Axis

newtype AxisFn = AxisFn { makeAxis ∷ Double → Axis }

instance Default AxisFn where
    def = AxisFn f
      where
        f len = Axis pMap [] axis'
          where 
            axis' = fromVertices [0 ^& 0, len ^& 0]
            pMap = PointMap (const Nothing) (0, -1)

instance Default LineStyle where
    def = LineStyle 
        { _lineWidth = 0.02
        , _lineColor = opaque blue
        }

data AxisOpts = AxisOpts
    { _tickN ∷ {-# UNPACK #-} !Int
    , _minorTickN ∷ {-# UNPACK #-} !Int
    , _tickLen ∷ Double
    , _minorTickLen ∷ Double
    , _labelOffset ∷ Double
    , _hideLabel ∷ Bool
    , _hidePadding ∷ Bool
    }

makeLenses ''AxisOpts

instance Default AxisOpts where
    def = AxisOpts
        { _tickN = 5
        , _minorTickN = 4
        , _tickLen = 0.4
        , _minorTickLen = 0.2
        , _labelOffset = -0.5
        , _hideLabel = False
        , _hidePadding = False
        }

realAxis ∷ (Double, Double) → Double → AxisOpts → AxisFn
realAxis r pad' opt = AxisFn $ realAxis' r pad' opt

realAxis' ∷ (Double, Double) → Double → AxisOpts → Double → Axis
realAxis' r pad' opt len = Axis pMap labels axis'
  where
    pMap = linearMap (l, u) (pad', len-pad')
    axis' = axis len pad' $ opt & tickN .~ tickN'
    labels = zip labelP (map show $ enumFromThenTo l (l+step) u)
    (l, u, step) = autoSteps ((opt^.tickN)-1) r
    tickN' = truncate ((u - l) / step) + 1
    labelP = zip (enumFromThenTo pad' (pad'+stepLabel) (len-pad')) $ repeat (opt^.labelOffset)
    stepLabel = (len - 2*pad') / fromIntegral (tickN' - 1)

indexAxis ∷ Int → [String] → Double → AxisOpts → AxisFn
indexAxis num labels pad' opt = AxisFn $ indexAxis' num labels pad' opt

indexAxis' ∷ Int → [String] → Double → AxisOpts → Double → Axis
indexAxis' num labels pad' opt len = Axis pMap labels' axis'
  where
    axis' = axis len pad' $ opt & tickN .~ num
                                & minorTickN .~ 0
                                & minorTickLen .~ 0
    pMap = linearMap (1, fromIntegral num) (pad', len-pad')
    labels' = zip labelP labels
    labelP = zip (enumFromThenTo pad' (pad'+stepLabel) (len-pad')) $ repeat (opt^.labelOffset)
    stepLabel = (len - 2*pad') / fromIntegral (num - 1)

emptyAxis ∷ AxisFn
emptyAxis = AxisFn $ const $ Axis pMap [] mempty
  where 
    pMap = PointMap (const Nothing) (0, -1)

flipAxisFn ∷ AxisFn → AxisFn
flipAxisFn axisF = AxisFn $ do (Axis m labels diag) ← makeAxis axisF
                               let (labelP, label) = unzip labels
                                   newLabels = zip labelP $ reverse label
                               return $ Axis (flipMap m) newLabels diag

axis ∷ Double → Double → AxisOpts → Diagram B R2
axis len pad opt = l <> translateX pad (majorTicks <> minorTicks)
  where
    l = fromVertices [ 0 ^& 0, len ^& 0 ]
    majorTicks = ticks (len - 2*pad) (opt^.tickN) (opt^.tickLen)
    minorTicks = ticks (len - 2*pad) minorN (opt^.minorTickLen)
    minorN = ((opt^.minorTickN) + 1) * ((opt^.tickN) - 1) + 1

ticks ∷ Double → Int → Double → Diagram B R2
ticks len tickNum tickL = mconcat [ fromVertices [ x ^& 0, x ^& tickL ] | x ← ticksPos ] 
  where
    ticksPos = enumFromThenTo 0 step len
    step = len / (fromIntegral tickNum - 1)

text' ∷ String → Diagram B R2
text' s = stroke (textSVG' (TextOpts s lin2 INSIDE_H KERN False 1 1)) # fc black # lw 0
