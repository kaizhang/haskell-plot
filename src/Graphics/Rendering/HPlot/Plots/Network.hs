{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.HPlot.Plots.Network
    ( network
    ) where

import Diagrams.Prelude
import Data.Default
import Control.Lens hiding ((#))
import Data.Maybe
import Graphics.Rendering.HPlot.Types
import Graphics.Rendering.HPlot.Utils
import Graphics.Rendering.HPlot.Axes.Common
import qualified Data.HashMap.Strict as M

network :: [(String, (Double, Double))] -> [(String, String)] -> DelayPlot
network vs es mapXY = [edges, ps]
  where
    ps = position $ map (\(s, p) -> (p2.fromJust.runMap pMap $ p, circle 0.05)) vs
    pMap = compose mapXY
    m = M.fromList vs
    edges = mconcat.mapMaybe f $ es

    f (s, t) = do s' <- M.lookup s m >>= runMap pMap
                  t' <- M.lookup t m >>= runMap pMap
                  return $ fromVertices [p2 s', p2 t']
                
