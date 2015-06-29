module Diagrams.Plots.Dendrogram where

import AI.Clustering.Hierarchical
import Data.Function (on)
import Data.Tree (Tree(..))
import Data.Maybe
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.TwoD.Layout.Tree
import Control.Monad.State

import Diagrams.Plots.Utils

dendrogramLayout :: Double -> Double -> Double -> Dendrogram a -> Tree ((Maybe a, Maybe (Colour Double)), P2 Double)
dendrogramLayout w h cut tree = evalState (go tree) 0
  where
    go (Leaf x) = do
        horiz <- get
        put $ horiz + xSep
        return $! Node ((Just x, Just red), w ^& horiz) []
    go (Branch _ d l r) = do
        left <- go l
        horiz <- get
        put $ horiz + xSep
        right <- go r
        let col | d <= cut = Just red
                | otherwise = Nothing
        return $! Node ((Nothing, col), (w * (maxD - d) / maxD) ^& horiz) [left, right]
    maxD = case tree of
        Branch _ d _ _ -> d
        Leaf _ -> error "error"
    xSep = h / fromIntegral (2 * ((length . flatten) tree - 1))

--dendrogramToBTree :: Dendrogram a -> BTree (Maybe a)
--dendrogramToBTree (Branch _ left right) = BNode Nothing (dendrogramToBTree left) (dendrogramToBTree right)
--dendrogramToBTree (Leaf x) = BNode (Just x) Empty Empty

drawDendrogram :: Double -> Double -> Double -> Dendrogram a -> (a -> String) -> Diagram B
drawDendrogram w h cutHeight d show' = renderTree' f g' (dendrogramLayout w h cutHeight d)
  where
    f x | isNothing $ fst x = mempty
        | otherwise = alignedText 0 0.5 $ show' $ fromJust $ fst x
    g ((_, col1), a) ((_, col2), b) | ((||) `on` isNothing) col1 col2 = a ~~ b # lwO 1 # dashingO [4, 2] 1
                                    | otherwise = a ~~ b # lwO 1 # lc (fromJust col1)
    g' ((_, col1), a) ((_, col2), b) | ((||) `on` isNothing) col1 col2 = l # dashingO [4, 2] 1
                                     | otherwise = l # lc (fromJust col1)
      where
        (x1, y1) = unp2 a
        (x2, y2) = unp2 b
        l = fromVertices [x1 ^& y1, x1 ^& y2, x2 ^& y2] # lwO 1

{-
-- | reorder data
ordering :: [a] -> (a -> a -> Double) -> [a]
ordering xs f = members $ hclust UPGMA xs f
-}
