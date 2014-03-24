import Control.Lens
import Data.Default
import Graphics.Rendering.HPlot
import Graphics.Rendering.Chart

p_opt :: PointOption
p_opt = p_shape .~ '^'
    $ p_col .~ "red"
    $ p_radius .~ 4
    $ def

l_opt :: LineOption
l_opt = l_type .~ 6
    $ def

main = plot' def (sequence [points p_opt, line l_opt] (Nothing,sample)) "test2.png"
