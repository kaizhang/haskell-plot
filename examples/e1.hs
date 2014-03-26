import Control.Lens
import Data.Default
import Graphics.Rendering.HPlot
import Data.DataSets (johnsonjohnson)

main = do
    (names, x:y:_) <- johnsonjohnson

    plot' (
        title .~ "Quarterly Earnings per Johnson & Johnson" 
        $ xlab .~ head names
        $ ylab .~ (names!!1)
        $ def) ( sequence [
            line def,
            points (
                p_col .~ "red" 
                $ p_shape .~ 'x'
                $ def)] (Just x, y) ) "e1.png"
