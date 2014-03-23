Haskell-Plot
=================

Simple plot interface based on [haskell-chart](https://github.com/timbod7/haskell-chart).

Examples:

```haskell
import Control.Lens
import Data.Default
import Graphics.Rendering.HPlot

main = hist' sample (
        hist_common .~ (
            title .~ "Histogram of Height"
            $ xlab .~ "Individual Height"
            $ col .~ "royalblue"
            $ opacity .~ 0.8 
            $ def) 
        $ def) "test1.png"
```

![](examples/test1.png)

```haskell
import Control.Lens
import Data.Default
import Graphics.Rendering.HPlot

main = points' (Nothing,sample) def "test2.png"
```

![](examples/test2.png)
