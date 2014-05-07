Haskell-Plot
=================

Header
=======

> import Diagrams.Prelude
> import Diagrams.Backend.SVG
> import Data.Default
> import System.Random
> import Graphics.Rendering.HPlot

> xs :: [Double]
> xs = take 50 $ randomRs (-100, 100) $ mkStdGen 2
>
> ys :: [Double]
> ys = take 50 $ randomRs (-100, 100) $ mkStdGen 4
>
> xAxis :: AxisFn
> xAxis = realAxis (minimum xs, maximum xs) 0.2 def
>
> yAxis :: AxisFn
> yAxis = realAxis (minimum ys, maximum ys) 0.2 def


> area :: PlotArea
> area = plotArea 5.5 4.8
>        ( def -- left axis
>        , xAxis
>        , yAxis
>        , def -- bottom axis
>        )

> ps = points xs ys def
> ts = ticks xs ys def # lc red

> plot = area <+ (ps, TR) <+ (ts, TR)

> main = renderSVG "tickPlot.svg" (Dims 480 480) $ showPlot plot
