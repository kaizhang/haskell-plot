Haskell-Plot
=================

I'm starting from scratch to write a new plotting library based on [Diagrams](http://projects.haskell.org/diagrams/). The old package can be found in the "Old" directory.

Header
=======

> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE UnicodeSyntax #-}

> import Diagrams.Prelude
> import Diagrams.Backend.SVG
> import Data.Default
> import System.Random
> import Graphics.Rendering.HPlot

Make axis
=========

Currently the **Axis** type contains three components: 

1. point map
2. labels and their positions
3. the actual axis with type "Diagram B R2"

We usually do not create **Axis** directly, instead, we creat a function which take a number (the length of axis) and generate the axis. Such functions are wrapped in the **AxisFn** type. **AxisFn**s are building blocks of chart. I wrote some general functions to help create **AxisFn**, i.e., realAxis, indexAxis, emptyAxis.

> xs ∷ [Double]
> xs = take 50 $ randomRs (-100, 100) $ mkStdGen 2
>
> ys ∷ [Double]
> ys = take 50 $ randomRs (-100, 100) $ mkStdGen 4
>
> xAxis ∷ AxisFn
> xAxis = realAxis (minimum xs, maximum xs) 0.2 def
>
> yAxis ∷ AxisFn
> yAxis = realAxis (minimum ys, maximum ys) 0.2 def

The **PlotArea** contains four axes: left axis, top axis, right axis and bottom axis. We can use **plotArea** to create a **PlotArea**.

> area ∷ PlotArea
> area = plotArea 5.5 4.8
>        ( yAxis  -- left axis
>        , def  -- top axis, using default axis which is a line
>        , def  -- right axis
>        , xAxis -- bottom axis
>        )

**PlotArea** can be converted to a Diagram by **showPlot**.

> areaDiag ∷ Diagram B R2
> areaDiag = (showPlot area) 

![](area.svg)

Now that we have the plotArea, we can start adding actual plots. For example, we can use points to make point plot:

> ps = points xs ys def

> pointPlot = area <+ (ps, BL) -- attach plot to plot area according to bottom and left axes

![](points.svg)

You can attach any number of plots to plotArea:

> ls = line xs ys def
> linePointPlot = area <+ (ps, BL) <+ (ls, BL)

![](lp.svg)

You can create an indexed Axis by indexAxis:

> bottomAxis = indexAxis 50 [] 0.2 def

Now let's create a plot area with 3 axes: left, bottom and right

> ys' ∷ [Double]
> ys' = take 50 $ randomRs (-1, 1) $ mkStdGen 2
>
> yAxis' = realAxis (minimum ys', maximum ys') 0.2 def
>
> area' = plotArea 5.5 4.8 (yAxis, def, yAxis', bottomAxis)
>
> l1 = line Nothing ys def # lc green
> l2 = line Nothing ys' def # lc red
>
> plot = area' <+ (l1, BL) <+ (l2, BR)

![](doublePlot.svg)

Note that the green line is placed according to left axis, and the red line is placed according to right axis.

> main = renderSVG "doublePlot.svg" (Dims 480 480) $ showPlot plot
