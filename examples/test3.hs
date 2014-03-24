import Control.Lens
import Data.Default
import Graphics.Rendering.HPlot

flower' = (_1 %~ Just $ flower)

main = plot' def (sequence [
    points (p_col .~ "red" $ p_opacity .~ 0.5 $ def),
    line (l_col .~ "royalblue" $ def)
    ] flower') "test3.png"
