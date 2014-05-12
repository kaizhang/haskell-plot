{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.HPlot.Axis
    ( AxisFn(..)
    , LabelOpts
    , Axis(..)
    , axisMap
    , axisLabels
    , axisDiag
    , axisLabelOpts
    , labelOffsetX
    , labelOffsetY
    , labelRotation
    ) where

import Diagrams.Prelude hiding (pad)
import Diagrams.Backend.SVG
import Data.Default
import Control.Lens (makeLenses)
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

{-
flipAxisFn :: AxisFn -> AxisFn
flipAxisFn axisF = AxisFn $ do (Axis m labels diag) <- makeAxis axisF
                               let (labelP, label) = unzip labels
                                   newLabels = zip labelP $ reverse label
                               return $ Axis (flipMap m) newLabels diag
                               -}

