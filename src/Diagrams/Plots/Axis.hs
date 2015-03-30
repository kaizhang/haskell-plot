{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Diagrams.Plots.Axis
    ( AxisFn(..)
    , LabelOpt
    , Axis(..)
    , axisMap
    , axisLabels
    , axisDiag
    , axisLabelOpt
    , offsetX
    , offsetY
    , rotation
    , size
    , fontFamily
    , realAxis
    , indexAxis
    , emptyAxis
    , axis
    , tickLen
    , minorTickLen
    , labelOpt
    ) where

import Diagrams.Prelude hiding (pad,rotation)
import Data.Default
import Control.Lens (makeLenses, makeFields, (^.))

import Diagrams.Plots.Types
import Diagrams.Plots.Utils

-- | control the rendering of labels
data LabelOpt = LabelOpt
    { _labelOptOffsetX :: !Double
    , _labelOptOffsetY :: !Double
    , _labelOptRotation :: !Double
    , _labelOptSize :: !Double
    , _labelOptFontFamily :: !String
    } deriving (Show)

makeFields ''LabelOpt

instance Default LabelOpt where
    def = LabelOpt
        { _labelOptOffsetX = 0
        , _labelOptOffsetY = -0.1
        , _labelOptRotation = 0
        , _labelOptSize = 0.2
        , _labelOptFontFamily  = "helvetica"
        }

data AxisOpt = AxisOpt
    { _nTick :: !Int
    , _nMinorTick :: !Int
    , _tickLen :: !Double
    , _minorTickLen :: !Double
    , _labelOpt :: !LabelOpt
    }

makeLenses ''AxisOpt

instance Default AxisOpt where
    def = AxisOpt
        { _nTick = 5
        , _nMinorTick = 4
        , _tickLen = 0.1
        , _minorTickLen = 0.05
        , _labelOpt = def
        }

-- | axis data type
data Axis = Axis
    { _axisMap :: !(PointMap Double)
    , _axisDiag :: !DiaR2
    , _axisLabels :: ![((Double, Double), String)]
    , _axisLabelOpt :: !LabelOpt
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


realAxis :: (Double, Double) -> Double -> AxisOpt -> AxisFn
realAxis r pad' opt = AxisFn ( \len ->
    let pMap = linearMap (fromRational l, fromRational u) (pad', len-pad')
        (l, u, step) = autoSteps ((opt^.nTick)-1) r
        axis' = lwO 1 $ axis len pad' $ opt & nTick .~ tickN'
        labels = zip labelP
            $ map ((show :: Float -> String) . fromRational) [l, l+step .. u]
        tickN' = truncate ((u - l) / step) + 1
        labelP = zip (enumFromThenTo pad' (pad'+stepLabel) (len-pad')) $ repeat 0
        stepLabel = (len - 2*pad') / fromIntegral (tickN' - 1)
    in Axis pMap axis' labels (opt^.labelOpt) )

indexAxis :: Int -> [String] -> Double -> AxisOpt -> AxisFn
indexAxis num labels pad' opt = AxisFn
    ( \len -> let axis' = axis len pad' $ opt & nTick .~ num
                                              & nMinorTick .~ 0
                                              & minorTickLen .~ 0
                  pMap = linearMap (1, fromIntegral num) (pad', len-pad')
                  labels' = zip labelP labels
                  labelP = zip (enumFromThenTo pad' (pad'+stepLabel) (len-pad')) $ repeat 0
                  stepLabel = (len - 2*pad') / fromIntegral (num - 1)
              in Axis pMap axis' labels' (opt^.labelOpt)
    )

emptyAxis :: AxisFn
emptyAxis = AxisFn $ const $ Axis pMap mempty [] def
  where 
    pMap = PointMap (const Nothing) (0, -1)

axis :: Double -> Double -> AxisOpt -> DiaR2
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
