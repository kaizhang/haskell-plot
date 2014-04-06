import Graphics.Rendering.HPlot
import Data.DataSets (anscombe)
import Data.Vector

main = do
    (labs, xs) <- anscombe

    heatmap' (fmap toList xs) (with & labRow .~ labs) "e4.png"
