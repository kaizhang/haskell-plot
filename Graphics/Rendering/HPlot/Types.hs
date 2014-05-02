{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Graphics.Rendering.HPlot.Types where

import Data.Maybe
import Diagrams.Prelude
import Diagrams.Backend.SVG

type DelayPlot = (PointMap Double, PointMap Double) → [Diagram B R2]

-- | mapping between points
data PointMap a = PointMap
    { runMap ∷ a → Maybe a
    , domain ∷ (a, a)
    }

compose ∷ (PointMap a, PointMap a) → PointMap (a,a)
compose ( PointMap m1 (l, u), PointMap m2 (l', u') ) = PointMap mapFn dom
  where
    mapFn (x,y) = do x' ← m1 x
                     y' ← m2 y
                     return (x', y')
    dom = ((l, l'), (u, u'))

flipMap ∷ PointMap Double → PointMap Double
flipMap (PointMap f (l, u)) = PointMap mapFn (l, u)
  where
    mapFn x = do x' ← f x
                 return (u' - x' + l')
    l' = fromJust.f $ l
    u' = fromJust.f $ u

class PlotData m a where
    getValues ∷ m a → [Double]

instance PlotData [] Double where
    getValues = id

instance PlotData Maybe a where
    getValues _ = [1.0..]

instance PlotData [] String where
    getValues _ = [1.0..]
