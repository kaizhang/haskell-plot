{-# LANGUAGE OverloadedStrings, UnicodeSyntax, TemplateHaskell #-}

module Graphics.Rendering.HPlot.HeatMap.Internal where

import Control.Monad
import Data.Colour
import Data.Default
import Graphics.Rendering.Chart
import Control.Lens

data HeatMap x = HeatMap {
      _heat_map_palette ∷ [Colour Double]
    , _heat_map_opacity ∷ Double
    , _heat_map_space ∷ Double
    -- | The matrix to be plotted
    , _heat_map_values ∷ [[x]]
}

makeLenses ''HeatMap

instance Default (HeatMap x) where
    def = HeatMap {
          _heat_map_palette = []
        , _heat_map_opacity = 1
        , _heat_map_space = 1
        , _heat_map_values = [[]]
    }

plotHeatMap ∷ PlotValue x ⇒ HeatMap x → Plot Double Double
plotHeatMap p = Plot {
    _plot_render = renderHeatMap p,
    _plot_legend = [],
    _plot_all_points = diagPoints p
}

renderHeatMap ∷ PlotValue x ⇒ HeatMap x → PointMapFn Double Double → ChartBackend ()
renderHeatMap p pmap = forM_ sqrs (\ ((i, j), y) → do
    let fstyle = FillStyleSolid $ withOpacity 
            (_heat_map_palette p !! scale' (toValue y))
            (_heat_map_opacity p)
    withFillStyle fstyle $ do
        pth ← alignStrokePath (sqrPath i j)
        fillPath pth
    )
    where
        sqrPath i j = let (x1, y1) = (i * (size + space), j * (size + space))
                          p1 = mapXY pmap (x1, y1)
                          p2 = mapXY pmap (x1 + size, y1 + size)
                      in rectPath (Rect p1 p2)

        nrows = fromIntegral $ length vals
        ncols = fromIntegral $ length $ head vals
        sqrs = zip [ (i, j) | j ← [nrows-1,nrows-2..0], i ← [0..ncols-1] ] vals'
        vals = _heat_map_values p
        vals' = concat vals
        size = 15
        space = _heat_map_space p
        min' = toValue $ minimum vals'
        max' = toValue $ maximum vals'
        n = fromIntegral $ length $ _heat_map_palette p
        scale' v = round $ (n - 1) * (v - min') / (max' - min')

diagPoints ∷ HeatMap x → ([Double], [Double])
diagPoints p = (0 : ncols * size + (ncols-1) * space : xs, 0 : nrows * size + (nrows-1) * space : ys)
    where
        (xs, ys) = ([0.5*size,1.5*size+space..(ncols-1)*(size+space)+0.5*size], [0.5*size,1.5*size+space..(nrows-1)*(size+space)+0.5*size])
        nrows = fromIntegral $ length vals
        ncols = fromIntegral $ length $ head vals
        vals = _heat_map_values p
        size = 15
        space = _heat_map_space p
