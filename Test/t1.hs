import Control.Lens
import Data.Default
import Graphics.Rendering.HPlot
import Data.DataSets (johnsonjohnson)

main = do
    (names, x:y:_) <- johnsonjohnson

    plot (
        title .~ "Quarterly Earnings per Johnson & Johnson" 
        $ xlab .~ head names
        $ ylab .~ (names!!1)
        $ grid .~ 'x'
        $ ylim .~ (2,10)
        $ def) ( sequence [
            line def,
            points (
                col .~ "red" 
                $ shape .~ 'x'
                $ def)] (Just x, y) )