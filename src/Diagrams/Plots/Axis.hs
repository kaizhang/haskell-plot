{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Diagrams.Plots.Axis
    ( AxisFn(..)
    , LabelOpts
    , Axis(..)
    , axisMap
    , axisLabels
    , axisDiag
    , axisLabelOpts
    , offsetX
    , offsetY
    , rotation
    , realAxis
    , indexAxis
    , emptyAxis
    , axis
    , tickLen
    , minorTickLen
    , labelOpts
    ) where

import Diagrams.Prelude hiding (pad,rotation)
import Data.Default
import Control.Lens (makeLenses, makeFields, (^.))

import Diagrams.Plots.Types
import Diagrams.Plots.Utils

-- | control the rendering of labels
data LabelOpts = LabelOpts
    { _labelOffsetX :: !Double
    , _labelOffsetY :: !Double
    , _labelRotation :: !Double
    } deriving (Show)

makeFields ''LabelOpts

instance Default LabelOpts where
    def = LabelOpts
        { _labelOffsetX = 0
        , _labelOffsetY = -0.1
        , _labelRotation = 0
        }

data AxisOpts = AxisOpts
    { _nTick :: !Int
    , _nMinorTick :: !Int
    , _tickLen :: !Double
    , _minorTickLen :: !Double
    , _labelOpts :: !LabelOpts
    }

makeLenses ''AxisOpts

instance Default AxisOpts where
    def = AxisOpts
        { _nTick = 5
        , _nMinorTick = 4
        , _tickLen = 0.1
        , _minorTickLen = 0.05
        , _labelOpts = def
        }

type Text = DiaR2

-- | axis data type
data Axis = Axis
    { _axisMap :: !(PointMap Double)
    , _axisDiag :: !DiaR2
    , _axisLabels :: ![((Double, Double), Text)]
    , _axisLabelOpts :: !LabelOpts
    }

makeLenses ''Axis

-- | given the length, draw axis
newtype AxisFn = AxisFn { makeAxis :: Double -> Axis }

instance Default AxisFn where
    def = emptyAxis 

{-
flipAxisFn :: AxisFn -> AxisFn
flipAxisFn axisF = AxisFn $ do (Axis m labels diag) <- makeAxis axisF
                               let (labelP, label) = unzip labels
                                   newLabels = zip labelP $ reverse label
                               return $ Axis (flipMap m) newLabels diag
                               -}


realAxis :: (Double, Double) -> Double -> AxisOpts -> AxisFn
realAxis r pad' opt = AxisFn 
    ( \len -> let pMap = linearMap (fromRational l, fromRational u) (pad', len-pad')
                  axis' = lwO 1 $ axis len pad' $ opt & nTick .~ tickN'
                  labels = zip labelP (map (text'.show.fromRational) $ enumFromThenTo l (l+step) u)
                  (l, u, step) = autoSteps ((opt^.nTick)-1) r
                  tickN' = truncate ((u - l) / step) + 1
                  labelP = zip (enumFromThenTo pad' (pad'+stepLabel) (len-pad')) $ repeat 0
                  stepLabel = (len - 2*pad') / fromIntegral (tickN' - 1)
              in Axis pMap axis' labels (opt^.labelOpts)
    )

indexAxis :: Int -> [String] -> Double -> AxisOpts -> AxisFn
indexAxis num labels pad' opt = AxisFn
    ( \len -> let axis' = axis len pad' $ opt & nTick .~ num
                                              & nMinorTick .~ 0
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
    majorTicks = ticks (len - 2*pad) (opt^.nTick) (opt^.tickLen)
    minorTicks = ticks (len - 2*pad) minorN (opt^.minorTickLen)
    minorN = ((opt^.nMinorTick) + 1) * ((opt^.nTick) - 1) + 1

ticks :: Double -> Int -> Double -> DiaR2
ticks len tickNum tickL = mconcat [ fromVertices [ x ^& 0, x ^& tickL ] | x <- ticksPos ] 
  where
    ticksPos = enumFromThenTo 0 step len
    step = len / (fromIntegral tickNum - 1)
