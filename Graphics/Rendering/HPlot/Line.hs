{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module Graphics.Rendering.HPlot.Line 
    ( line
    , LineOpts
    , lineshape
    ) where

import Diagrams.Prelude
import Data.Default
import Control.Lens (makeLenses, (^.))
import Data.Maybe
import Graphics.Rendering.HPlot.Types

data LineOpts = LineOpts
    { _lineshape ∷ Char
    }

makeLenses ''LineOpts

instance Default LineOpts where
    def = LineOpts
        { _lineshape = 'o'
        }

line ∷ (PlotData m1 a1, PlotData m2 a2) ⇒ m1 a1 → m2 a2 → LineOpts → DelayPlot
line xs ys opt (mapX, mapY) = [l]
  where
    l = fromVertices.map p2.mapMaybe (runMap pMap) $ xy
    xy = zip (getValues xs) $ getValues ys
    pMap = compose (mapX, mapY)
