{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG
import System.Random
import Graphics.Rendering.HPlot

g1 ∷ StdGen
g1 = mkStdGen 1

g2 ∷ StdGen
g2 = mkStdGen 12

xs ∷ [Double]
xs = take 50 $ randomRs (0,300) g1

ys ∷ [Double]
ys = take 50 $ randomRs (0,50) g2

labels ∷ [String]
labels = ["John", "Marry", "Richard", "W", "CC"]

main = do
    let bAxis = indexAxis (length ys) [] 1 with
        rAxis = realAxis (minimum ys, maximum ys) 4 with
        lAxis = realAxis (minimum xs, maximum xs) 2 with
        tAxis = emptyAxis
        p = plotArea 40 30 (lAxis, tAxis, rAxis, bAxis)
        ps1 = points labels ys with # fc red
        ps2 = points labels xs with # fc red
        ln1 = line labels ys with # lc green # lw 0.05
        ln2 = line labels xs with # lc red # lw 0.1
        bs = (bars labels ys $ with & barBaseLine .~ Just 0 & barOrientation .~ '^') # fc blue

        p' = placeOn ln1 p BR
        p'' = placeOn ln2 p' BL
        p''' = placeOn bs p'' BR


        c = circle 4

    renderSVG "1.svg" (Dims 500 500) $ showPlot p''' <> c

