{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.HPlot.Axis
   ( AxisFn(..)
   , Axis
   , axisMap
   , axisLabels
   , axisDiag
   , axisLabelOpts
   , labelOffsetX
   , labelOffsetY
   , labelRotation
   , realAxis
   , indexAxis
   , emptyAxis
   , axis
   , text'
   , tickLen
   , minorTickLen
   , labelOpts
   ) where

import Diagrams.Prelude hiding (pad)
import Diagrams.Backend.SVG
import Data.Default
import Control.Lens (makeLenses, (^.))
import Graphics.SVGFonts.ReadFont
import Graphics.Rendering.HPlot.Utils
import Graphics.Rendering.HPlot.Types

data LabelOpts = LabelOpts
    { _labelOffsetX :: Double
    , _labelOffsetY :: Double
    , _labelRotation :: Double
    }

makeLenses ''LabelOpts

instance Default LabelOpts where
    def = LabelOpts
        { _labelOffsetX = 0
        , _labelOffsetY = -0.1
        , _labelRotation = 0
        }

type Text = Diagram B R2

data Axis = Axis
    { _axisMap :: PointMap Double
    , _axisDiag :: Diagram B R2
    , _axisLabels :: [((Double, Double), Text)]
    , _axisLabelOpts :: LabelOpts
    }

makeLenses ''Axis

newtype AxisFn = AxisFn { makeAxis :: Double -> Axis }

instance Default AxisFn where
    def = AxisFn f
      where
        f len = Axis pMap axis' [] def
          where 
            axis' = fromVertices [0 ^& 0, len ^& 0]
            pMap = PointMap (const Nothing) (0, -1)

data AxisOpts = AxisOpts
    { _tickN :: {-# UNPACK #-} !Int
    , _minorTickN :: {-# UNPACK #-} !Int
    , _tickLen :: Double
    , _minorTickLen :: Double
    , _labelOpts :: LabelOpts
    }

makeLenses ''AxisOpts

instance Default AxisOpts where
    def = AxisOpts
        { _tickN = 5
        , _minorTickN = 4
        , _tickLen = 0.1
        , _minorTickLen = 0.05
        , _labelOpts = def
        }

realAxis :: (Double, Double) -> Double -> AxisOpts -> AxisFn
realAxis r pad' opt = AxisFn 
    ( \len -> let pMap = linearMap (l, u) (pad', len-pad')
                  axis' = axis len pad' $ opt & tickN .~ tickN'
                  labels = zip labelP (map (text'.show) $ enumFromThenTo l (l+step) u)
                  (l, u, step) = autoSteps ((opt^.tickN)-1) r
                  tickN' = truncate ((u - l) / step) + 1
                  labelP = zip (enumFromThenTo pad' (pad'+stepLabel) (len-pad')) $ repeat 0
                  stepLabel = (len - 2*pad') / fromIntegral (tickN' - 1)
              in Axis pMap axis' labels (opt^.labelOpts)
    )

indexAxis :: Int -> [String] -> Double -> AxisOpts -> AxisFn
indexAxis num labels pad' opt = AxisFn
    ( \len -> let axis' = axis len pad' $ opt & tickN .~ num
                                              & minorTickN .~ 0
                                              & minorTickLen .~ 0
                  pMap = linearMap (1, fromIntegral num) (pad', len-pad')
                  labels' = zip labelP $ map text' labels
                  labelP = zip (enumFromThenTo pad' (pad'+stepLabel) (len-pad')) $ repeat 0
                  stepLabel = (len - 2*pad') / fromIntegral (num - 1)
              in Axis pMap axis' labels' (opt^.labelOpts)
    )

emptyAxis :: AxisFn
emptyAxis = AxisFn $ const $ Axis pMap mempty [] def
  where 
    pMap = PointMap (const Nothing) (0, -1)

{-
flipAxisFn :: AxisFn -> AxisFn
flipAxisFn axisF = AxisFn $ do (Axis m labels diag) <- makeAxis axisF
                               let (labelP, label) = unzip labels
                                   newLabels = zip labelP $ reverse label
                               return $ Axis (flipMap m) newLabels diag
                               -}


axis :: Double -> Double -> AxisOpts -> Diagram B R2
axis len pad opt = l <> translateX pad (majorTicks <> minorTicks)
  where
    l = fromVertices [ 0 ^& 0, len ^& 0 ]
    majorTicks = ticks (len - 2*pad) (opt^.tickN) (opt^.tickLen)
    minorTicks = ticks (len - 2*pad) minorN (opt^.minorTickLen)
    minorN = ((opt^.minorTickN) + 1) * ((opt^.tickN) - 1) + 1

ticks :: Double -> Int -> Double -> Diagram B R2
ticks len tickNum tickL = mconcat [ fromVertices [ x ^& 0, x ^& tickL ] | x <- ticksPos ] 
  where
    ticksPos = enumFromThenTo 0 step len
    step = len / (fromIntegral tickNum - 1)

text' :: String -> Diagram B R2
text' s = stroke (textSVG' (TextOpts s lin2 INSIDE_H KERN False 0.2 0.2)) # fc black # lw 0
