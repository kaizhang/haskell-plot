import Control.Lens
import Data.Default
import Graphics.Rendering.HPlot
import Data.DataSets (anscombe)
import Data.Vector

main = do
    (labs, xs) <- anscombe

    heatmap' (labRow .~ labs $ def) (fmap toList xs) "e4.png"
