{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Rendering.HPlot.Points where

import qualified Data.Foldable as F
import Graphics.Rendering.Chart
import Control.Lens
import Data.Default
import Graphics.Rendering.HPlot.Types
import Graphics.Rendering.HPlot.Utils
import Data.Maybe

toPointStyle ∷ PointOption → PointStyle
toPointStyle opt = case () of
    _ | s `elem` ".●" → filledCircles r color
      | s `elem` "o○" → hollowCircles r thick color
      | s `elem` "x" → exes r thick color
      | s `elem` "+" → plusses r thick color
      | s `elem` "*" → stars r thick color
      | s `elem` "v▼" → filledPolygon r 3 True color
      | s `elem` "^▲" → filledPolygon r 3 False color
      | s `elem` "#■" → filledPolygon r 4 False color
      | s `elem` "◆" → filledPolygon r 4 True color
      | otherwise → filledCircles r color
        where
            s = opt^.shape
            r = opt^.radius
            color = mkColor (opt^.col) (opt^.opacity)
            thick = opt^.lwd

points ∷ F.Foldable f ⇒ PointOption → (Maybe (f Double), f Double) → EitherPlot
points opt (x, y) | isNothing x = Left $ mkPlot $ addIndexes y'
                  | otherwise = Right $ mkPlot $ zip (F.toList $ fromJust x) y'
    where
        y' = F.toList y
        mkPlot x_y = toPlot $ plot_points_values .~ x_y
            $ plot_points_style .~ toPointStyle opt
            $ def
