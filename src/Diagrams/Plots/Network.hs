{-# LANGUAGE TemplateHaskell #-}

module Diagrams.Plots.Network
    ( network
    ) where

import Diagrams.Prelude
import Data.Default
import Control.Lens hiding ((#))
import Data.Maybe
import qualified Data.HashMap.Strict as M

import Diagrams.Plots.Types
import Diagrams.Plots.Utils
import Diagrams.Plots.Axis

network :: [(String, (Double, Double))] -> [(String, String)] -> PlotFn
network vs es mapX mapY = [edges, ps]
  where
    ps = position $ map (\(s, p) -> (p2.fromJust.runMap pMap $ p, circle 0.05)) vs
    pMap = compose mapX mapY
    m = M.fromList vs
    edges = mconcat.mapMaybe f $ es

    f (s, t) = do s' <- M.lookup s m >>= runMap pMap
                  t' <- M.lookup t m >>= runMap pMap
                  return $ fromVertices [p2 s', p2 t']
