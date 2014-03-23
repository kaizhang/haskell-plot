import Type
import Hist
import Control.Lens
import DataSets
import Data.Default

main = hist' sample (
        common .~ (
            title .~ "Histogram of Height"
            $ xlab .~ "Individual Height"
            $ col .~ "royalblue"
            $ opacity .~ 0.8 
            $ def)
        $ breaks .~ riceRule
        $ def) "test1.png"
