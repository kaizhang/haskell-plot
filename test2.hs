import Type
import Point
import Control.Lens
import DataSets
import Data.Default

main = points' (Nothing,sample) (
        common .~ (
            xlab .~ "Individual Height"
            $ col .~ "red"
            $ def)
        $ def) "test2.png"

