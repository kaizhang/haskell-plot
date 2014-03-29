import Control.Lens
import Data.Default
import Graphics.Rendering.HPlot
import Data.DataSets (rivers)

main = do
    (_, x:_) <- rivers

    hist' (
        title .~ "Lengths of Major North American Rivers" 
        $ xlab .~ "length"
        $ col .~ "royalblue"
        $ opacity .~ 0.8
        $ def) x "e2.png"
