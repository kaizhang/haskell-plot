import Control.Lens
import Data.Default
import Graphics.Rendering.HPlot

main = hist' (
    h_col .~ "royalblue"
    $ h_opacity .~ 0.8
    $ def) sample "test1.png"
