import Graphics.Rendering.HPlot
import Data.DataSets (worldphones)

main = do
    (label, ys) <- worldphones
    let bs = bars (Nothing, ys) $ with & legend .~ label
    plot' [bs] ( with & labels .~ map show [1951..1961]
                      & title .~ "World phones data: log scale for response"
                      & xlab .~ "Years"
                      & ylab .~ "Number of telephones (1000's)"
                      ) "e3.png"

