Histogram-haskell
=================

Making histogram in Haskell

Example:

```haskell
{-# LANGUAGE OverloadedStrings, UnicodeSyntax, TemplateHaskell, BangPatterns #-}

import Type
import Hist
import Control.Lens
import DataSets
import Data.Default.Class


main = hist' sample (
        common .~ (
            title .~ "Histogram of Height"
            $ xlab .~ "Individual Height"
            $ col .~ ["royalblue"]
            $ opacity .~ 0.8 
            $ def) 
        $ def) "test1.png"
```

![](test1.png)
