import Control.Lens
import Data.Default
import Graphics.Rendering.HPlot

main = points (Nothing,sample) 
    $ shape .~ '*'
    $ thickness .~ 1
    $ common .~ (
        col .~ "red"
        $ def)
    $ def
