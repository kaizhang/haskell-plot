import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Data.Default
import Control.Lens ((^.))
import Data.List
import Data.Ord

import Diagrams.Plots

-- | project a 3d point to 2d
projection :: (Double, Double, Double)  -- ^ position of camera
           -> (Double, Double, Double)  -- ^ orientation of camera
           -> (Double, Double, Double)  
           -> (Double, Double, Double)  -- ^ 3d point to be projected
           -> (Double, Double)
projection (cx',cy',cz') (θx,θy,θz) (ex,ey,ez) (ax,ay,az) = (bx, by)
  where
    bx = ez / dz * dx - ex
    by = ez / dz * dy - ey
    dx = cy * (sz * y + cz * x) - sy * z
    dy = sx * (cy * z + sy * (sz * y + cz * x)) + cx * (cz * y - sz * x)
    dz = cx * (cy * z + sy * (sz * y + cz * x)) - sx * (cz * y - sz * x)
    x = ax - cx'
    y = ay - cy'
    z = az - cz'
    sx = sin θx
    sy = sin θy
    sz = sin θz
    cx = cos θx
    cy = cos θy
    cz = cos θz

ps :: [(Double, Double, Double)]
ps = [(2,2,2), (2,5,2), (5,5,2), (5,2,2)]

ps' :: [(Double, Double, Double)]
ps' = [(3,3,1), (3,4,1), (4,4,1), (4,3,1)]

nodes = zipWith f [0..] $ map (projection (0,0,0) (0,0,0) (0,0,1)) $ ps ++ ps'
  where
    f i (x,y) = def {_nodeId = i, _nodeX = x, _nodeY = y}

{-
nodes = [ def { _nodeId=0, _nodeX=2, _nodeY=2, _nodeSize=0.1, _nodeOpacity=0.5, _nodeColor=red }
        , def { _nodeId=1, _nodeX=2, _nodeY=5, _nodeSize=0.03 }
        , def { _nodeId=2, _nodeX=5, _nodeY=5, _nodeColor=green }
        , def { _nodeId=3, _nodeX=5, _nodeY=2, _nodeColor=green }
        ]
        -}

edges = [ def { _edgeId = 0, _edgeFrom = 0, _edgeTo = 1 }
        , def { _edgeId = 1, _edgeFrom = 1, _edgeTo = 2 }
        , def { _edgeId = 3, _edgeFrom = 2, _edgeTo = 3 }
        , def { _edgeId = 4, _edgeFrom = 3, _edgeTo = 0 }
        
        , def { _edgeId = 5, _edgeFrom = 4, _edgeTo = 5 }
        , def { _edgeId = 6, _edgeFrom = 5, _edgeTo = 6 }
        , def { _edgeId = 7, _edgeFrom = 6, _edgeTo = 7 }
        , def { _edgeId = 8, _edgeFrom = 7, _edgeTo = 4 }
        ]

main = do
    let xaxis = realAxis (xlo,xhi) 0.2 def
        yaxis = realAxis (ylo,yhi) 0.2 def
        area = plotArea 5.5 4.8 (yaxis, emptyAxis, emptyAxis, xaxis)
        plot = drawGraph (nodes, edges)
        p = area <+ (plot, BL)
        xlo = _nodeX $ minimumBy (comparing _nodeX) nodes
        xhi = _nodeX $ maximumBy (comparing _nodeX) nodes
        ylo = _nodeY $ minimumBy (comparing _nodeY) nodes
        yhi = _nodeY $ maximumBy (comparing _nodeY) nodes
    renderCairo "1.png" (Dims 480 480) $ showPlot p
