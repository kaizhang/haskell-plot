{-# LANGUAGE TemplateHaskell #-}

module Diagrams.Plots.Heatmap
    ( heatmap
    , HeatmapOpt
    , palette
    , range
    ) where

import Control.Lens (makeLenses, (^.))
import Data.Colour.Palette.BrewerSet (brewerSet, ColorCat(..))
import Data.Default (Default, def)
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import qualified Data.Vector as V

import Diagrams.Prelude
import Diagrams.Plots.Types
import Diagrams.Plots.Utils

data HeatmapOpt = HeatmapOpt
    { _palette :: ![Colour Double]
    , _range :: !(Maybe (Double, Double))
    }

instance Default HeatmapOpt where
    def = HeatmapOpt
        { _palette = reverse $ brewerSet RdYlBu 11
        , _range = Nothing
        }

makeLenses ''HeatmapOpt

heatmap :: [[Double]] -> HeatmapOpt -> PlotFn
heatmap mat opt mapX mapY = map (\((x,y), z) -> rect' z # moveTo (x ^& y)) hm
  where
    hm = zip [ (x,y) | y <- reverse (take nRow [oriY, oriY + stepY ..]), x <- take nCol [oriX, oriX + stepX ..] ]
       . map (fromJust . runMap (linearMapBound r (0,1)))
       $ mat'
    r = fromMaybe (minimum mat', maximum mat') $ _range opt
    mat' = concat mat
    stepX = (fromJust.runMap mapX) 2 - (fromJust.runMap mapX) 1
    stepY = (fromJust.runMap mapY) 2 - (fromJust.runMap mapY) 1
    rect' z = let col = colorMapSmooth z palette'
              in rect stepX stepY # lwO 0 # fc col
    palette' = V.fromList $ opt^.palette
    pMap = compose mapX mapY
    nRow = fromIntegral . length $ mat
    nCol = fromIntegral . length . head $ mat
    (oriX, oriY) = fromJust $ runMap pMap (1,1)

{-
colorKey :: Double -> Double -> (Double, Double) -> [Colour Double] -> DiaR2
colorKey w h r cs = vcat (map rect' [1,0.995..0])
  where
    rect' z = rect w (h/200) # lc (colorMap z cs) # fc (colorMap z cs)
    -}

colorMap :: Double -- a value from 0 to 1
         -> V.Vector (Colour Double) -> Colour Double
colorMap x colors | x == 1 = V.last colors
                  | otherwise = colors V.! (truncate $ x * n)
  where
    n = fromIntegral . V.length $ colors
{-# INLINE colorMap #-}

-- | map numbers to colors
colorMapSmooth :: Double -- a value from 0 to 1
               -> V.Vector (Colour Double) -> Colour Double
colorMapSmooth x colors = blend p (colors V.! i) $ colors V.! (i+1)
  where
    p = fromIntegral i - x * (fromIntegral n - 1) + 1
    i | x == 1 = n - 2
      | otherwise = truncate $ x * (fromIntegral n - 1)
    n = V.length colors
{-# INLINE colorMapSmooth #-}
