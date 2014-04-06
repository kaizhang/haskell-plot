import Graphics.Rendering.HPlot
import Data.DataSets (rivers)

main = do
    (_, x:_) <- rivers

    hist' x ( with & title .~ "Lengths of Major North American Rivers" 
                   & xlab .~ "length"
                   & col .~ "royalblue"
                   & opacity .~ 0.8 ) "e2.png"
