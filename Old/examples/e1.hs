import Graphics.Rendering.HPlot
import Data.DataSets (johnsonjohnson)

main = do
    (names, x:y:_) <- johnsonjohnson

    let ps = points (Just x, y) $ with & col .~ "green"
                                       & shape .~ 'o'
                                       & radius .~ 3
        l1 = line (Just x, y) $ with & lwd .~ 15
                                     & opacity .~ 0.2
                                     & col .~ "green"
        l2 = line (Just x, y) $ with & col .~ "black"

    plot' [l1, l2, ps] ( with & title .~ "Quarterly Earnings per Johnson & Johnson" 
                              & xlab .~ head names
                              & ylab .~ (names!!1)
                              & width .~ 800
                          ) "e1.png"
