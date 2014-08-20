{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.HPlot.Axes.Common
    ( realAxis
    , indexAxis
    , emptyAxis
    , axis
    , tickLen
    , minorTickLen
    , labelOpts
    ) where

import Diagrams.Prelude
import Graphics.Rendering.HPlot.Axis
import Graphics.Rendering.HPlot.Types
import Graphics.Rendering.HPlot.Utils
import Control.Lens hiding ((#))
import Data.Default

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
    ( \len -> let pMap = linearMap (fromRational l, fromRational u) (pad', len-pad')
                  axis' = lwO 1 $ axis len pad' $ opt & tickN .~ tickN'
                  labels = zip labelP (map (text'.show.fromRational) $ enumFromThenTo l (l+step) u)
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

axis :: Double -> Double -> AxisOpts -> DiaR2
axis len pad opt = l <> translateX pad (majorTicks <> minorTicks)
  where
    l = fromVertices [ 0 ^& 0, len ^& 0 ]
    majorTicks = ticks (len - 2*pad) (opt^.tickN) (opt^.tickLen)
    minorTicks = ticks (len - 2*pad) minorN (opt^.minorTickLen)
    minorN = ((opt^.minorTickN) + 1) * ((opt^.tickN) - 1) + 1

ticks :: Double -> Int -> Double -> DiaR2
ticks len tickNum tickL = mconcat [ fromVertices [ x ^& 0, x ^& tickL ] | x <- ticksPos ] 
  where
    ticksPos = enumFromThenTo 0 step len
    step = len / (fromIntegral tickNum - 1)
