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
