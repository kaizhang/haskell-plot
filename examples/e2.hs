import Control.Lens
import Data.Default
import Graphics.Rendering.HPlot
import Data.DataSets (rivers)

main = do
    (_, x:_) <- rivers

    hist' (
        h_title .~ "Lengths of Major North American Rivers" 
        $ h_xlab .~ "length"
        $ h_col .~ "royalblue"
        $ h_opacity .~ 0.8
        $ def) x "e2.png"
