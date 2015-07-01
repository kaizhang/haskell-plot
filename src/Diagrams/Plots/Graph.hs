{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Diagrams.Plots.Graph
    ( Node(..)
    , Edge(..)
    , drawGraph
    ) where

import Control.Arrow ((&&&))
import Diagrams.Prelude
import Data.Default
import Data.Maybe
import qualified Data.HashMap.Strict as M

import Diagrams.Plots.Types

data Node = Node
    { _nodeId :: Int
    , _nodeLabel :: String
    , _nodeSize :: Double
    , _nodeColor :: Colour Double
    , _nodeOpacity :: Double
    , _nodeX :: Double
    , _nodeY :: Double
    , _nodeLw :: Double
    } deriving (Show)

instance Default Node where
    def = Node
        { _nodeId = 0
        , _nodeLabel = ""
        , _nodeSize = 0.05
        , _nodeColor = white
        , _nodeOpacity = 1
        , _nodeX = 0
        , _nodeY = 0
        , _nodeLw = 0.3
        }

data Edge = Edge
    { _edgeId :: Int
    , _edgeLabel :: String
    , _edgeColor :: Colour Double
    , _edgeFrom :: Int
    , _edgeTo :: Int
    , _edgeWeight :: Double
    } deriving (Show)

instance Default Edge where
    def = Edge
        { _edgeId = 0
        , _edgeLabel = ""
        , _edgeColor = black
        , _edgeFrom = 0
        , _edgeTo = 0
        , _edgeWeight = 0.3
        }

type Graph = ([Node], [Edge])

drawGraph :: Graph -> PlotFn
drawGraph (vs, es) mapX mapY = [ps, edges]
  where
    vs' = M.fromList . map (_nodeId &&& id) $ vs
    ps = position $ map drawNode vs
    pMap = compose mapX mapY
    edges = mconcat . mapMaybe drawEdge $ es

    drawNode n = ( p2 . fromJust . runMap pMap . (_nodeX &&& _nodeY) $ n
                 , circle (_nodeSize n) # fc (_nodeColor n)
                                        # opacity (_nodeOpacity n)
                                        # lwO (_nodeLw n)
                 )
    drawEdge e = do
        s <- M.lookup (_edgeFrom e) vs' >>= runMap pMap . (_nodeX &&& _nodeY)
        t <- M.lookup (_edgeTo e) vs' >>= runMap pMap . (_nodeX &&& _nodeY)
        return $ fromVertices [p2 s, p2 t] # lwO (_edgeWeight e)
