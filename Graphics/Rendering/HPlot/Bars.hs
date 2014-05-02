{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module Graphics.Rendering.HPlot.Bars 
    ( BarOpts
    , barWidth
    , barBaseLine
    , barOrientation
    , bars
    ) where

import Diagrams.Prelude
import Data.Default
import Control.Lens (makeLenses, (^.), both)
import Data.Maybe
import Graphics.Rendering.HPlot.Types

data BarOpts = BarOpts
    { _barWidth ∷ Double  -- ^ from 0 to 1
    , _barBaseLine ∷ Maybe Double
    , _barOrientation ∷ Char
    }

makeLenses ''BarOpts

instance Default BarOpts where
    def = BarOpts
        { _barWidth = 0.8
        , _barBaseLine = Nothing
        , _barOrientation = '^'
        }

bars ∷ (PlotData m1 a1, PlotData m2 a2) ⇒ m1 a1 → m2 a2 → BarOpts → DelayPlot
bars xs ys opt m = case opt^.barOrientation of
                       '^' → upBars xs ys opt m
                       '>' → rightBars xs ys opt m
                       'V' → downBars xs ys opt m
                       _ → upBars xs ys opt m

upBars ∷ (PlotData m1 a1, PlotData m2 a2) ⇒ m1 a1 → m2 a2 → BarOpts → DelayPlot
{-# INLINE upBars #-}
upBars xs ys opt (mapX, mapY) = map (uncurry moveTo) [ (x ^& ((y+bl)/2), rect w (y-bl)) | (x, y) ← xy ]
  where
    xy = mapMaybe (runMap pMap) $ zip (getValues xs) $ getValues ys
    w = (opt^.barWidth) * gap'
    gap' = (fromJust.runMap mapX) 2 - (fromJust.runMap mapX) 1
    pMap = compose (mapX, mapY)
    bl = fromMaybe 0 $ do b ← opt^.barBaseLine
                          runMap mapY b

rightBars ∷ (PlotData m1 a1, PlotData m2 a2) ⇒ m1 a1 → m2 a2 → BarOpts → DelayPlot
{-# INLINE rightBars #-}
rightBars xs ys opt (mapX, mapY) = map (uncurry moveTo) [ ( ((x+bl)/2) ^& y, rect (x-bl) h) | (x, y) ← xy ]
  where
    xy = mapMaybe (runMap pMap) $ zip (getValues xs) $ getValues ys
    h = (opt^.barWidth) * gap'
    gap' = (fromJust.runMap mapY) 2 - (fromJust.runMap mapY) 1
    pMap = compose (mapX, mapY)
    bl = fromMaybe 0 $ do b ← opt^.barBaseLine
                          runMap mapX b

downBars ∷ (PlotData m1 a1, PlotData m2 a2) ⇒ m1 a1 → m2 a2 → BarOpts → DelayPlot
{-# INLINE downBars #-}
downBars xs ys opt (mapX, mapY) = map (uncurry moveTo) [ (x ^& ((areaHeight+y-bl)/2), rect w (areaHeight-y-bl) ) | (x, y) ← xy ]
  where
    xy = mapMaybe (runMap pMap) $ zip (getValues xs) $ getValues ys
    w = (opt^.barWidth) * gap'
    gap' = (fromJust.runMap mapX) 2 - (fromJust.runMap mapX) 1
    pMap = compose (mapX, mapY)
    areaHeight = l' + u'
    (l', u') = both %~ fromJust . runMap mapY $ domain mapY
    bl = fromMaybe 0 $ do b ← opt^.barBaseLine
                          runMap mapY b
