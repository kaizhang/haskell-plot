import Graphics.Rendering.HPlot
import Data.DataSets (johnsonjohnson)

main = do
    (names, x:y:_) <- johnsonjohnson
    let ls = line (Just x, y) with

    plot [ls] $ with & title .~ "Quarterly Earnings per Johnson & Johnson" 
                   & xlab .~ head names
                   & ylab .~ (names!!1)
                   & grid .~ 'n'
                   & axes .~ 0
