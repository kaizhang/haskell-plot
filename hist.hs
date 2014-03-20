{-# LANGUAGE OverloadedStrings, UnicodeSyntax, BangPatterns, TemplateHaskell #-}

import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Statistics.Sample.Histogram
import qualified Data.Vector as V
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Colour
import Data.Colour.Names
import Control.Lens
import Data.Default.Class
import Statistics.Quantile

data HistOption = HistOption {
    _title ∷ String
    } deriving (Show)

makeLenses ''HistOption

--http://en.wikipedia.org/wiki/Freedman%E2%80%93Diaconis_rule
freedman_diaconis_rule ∷ [Double] → Int
freedman_diaconis_rule xs = round ((maximum xs - minimum xs) / binSzie)
    where
        binSzie = 2 * iqr xs * n**(-1/3)
        n = fromIntegral $ length xs
        iqr x = let x' = V.fromList x
                    quartile3 = weightedAvg 3 4 x' 
                    quartile1 = weightedAvg 1 4 x' 
                in quartile3 - quartile1

hist ∷ [Double] → String → IO (PickFn ())
hist xs = renderableToFile def (toRenderable layout)
    where
        layout = 
            layout_title .~ "Title"
            $ layout_plots .~ [plotBars bars]
            $ layout_x_axis . laxis_generate .~ autoIndexAxis (fmap show labels)
            $ def ∷ Layout PlotIndex Double
        bars = 
            plot_bars_values .~ addIndexes (fmap return $ counts)
            $ plot_bars_spacing .~ BarsFixGap 0 0
            $ plot_bars_alignment .~ BarsLeft
            $ def

        numBins = freedman_diaconis_rule xs
        labels = autoSteps numBins xs
        counts = V.toList $ histogram_ (length labels) (minimum labels) (maximum labels) (V.fromList xs)

main = do
    let a = [2,2,4,6,7,8,3,4,4,4,2,3,4,5,6,7,8,3]
    hist a "1.png"
