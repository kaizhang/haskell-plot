{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module Graphics.Rendering.HPlot.Points 
    ( points
    , PointOpts
    , shape
    ) where

import Diagrams.Prelude
import Data.Default
import Control.Lens (makeLenses, (^.))
import Data.Maybe
import Graphics.Rendering.HPlot.Types

data PointOpts = PointOpts
    { _shape ∷ Char
    }

makeLenses ''PointOpts

instance Default PointOpts where
    def = PointOpts
        { _shape = 'o'
        }

points ∷ (PlotData m1 a1, PlotData m2 a2) ⇒ m1 a1 → m2 a2 → PointOpts → DelayPlot
points xs ys opt (mapX, mapY) = map (uncurry moveTo) ps
  where
    ps = flip zip (repeat s).map p2.mapMaybe (runMap pMap) $ xy
    xy = zip (getValues xs) $ getValues ys
    s = stroke.getShape $ opt^.shape
    pMap = compose (mapX, mapY)

getShape ∷ Char → Path R2
{-# INLINE getShape #-}
getShape s | s == 'o' = circle 0.07
           | s == '^' = eqTriangle 0.1
           | s == '#' = square 0.1
           | s == '+' = plus 0.07
           | s == '*' = star (StarSkip 2) (pentagon 0.1)
           | s == 'x' = cross 0.07
           | otherwise = circle 0.07

cross ∷ Double → Path R2
{-# INLINE cross #-}
cross x = fromVertices [ x^&(-x) , (-x)^&x ]
          <> fromVertices [ x^&x , (-x)^&(-x) ]

plus ∷ Double → Path R2
{-# INLINE plus #-}
plus x = cross x # rotate (45 @@ deg)
