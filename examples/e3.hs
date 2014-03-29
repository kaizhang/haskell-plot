import Control.Lens
import Data.Default
import Graphics.Rendering.HPlot
import Data.DataSets (worldphones)

main = do
    (label, ys) <- worldphones

    plot' (labels .~ map show [1951..1961]
        $ title .~ "World phones data: log scale for response"
        $ xlab .~ "Years"
        $ ylab .~ "Number of telephones (1000's)"
        $ def) [
            bars (legend .~ label $ def) (Nothing, ys)] "e3.png"

