{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Rendering.HPlot.Hist (
    hist
    , hist'
    , freedmanDiaconis
    , squareRoot
    , riceRule
    )where

import qualified Data.Foldable as F
import qualified Data.Vector as V
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart
import Control.Lens
import Data.Default
import Graphics.Rendering.HPlot.Types
import Graphics.Rendering.HPlot.Bars
import Graphics.Rendering.HPlot.Plot
import Graphics.Rendering.HPlot.Utils

convertOpt ∷ HistOption → (PlotOption, BarOption)
convertOpt opt = (
    title .~ opt^.title
    $ xlab .~ opt^.xlab
    $ ylab .~ opt^.ylab
    $ xlim .~ opt^.xlim
    $ ylim .~ opt^.ylim
    $ width .~ opt^.width
    $ height .~ opt^.height
    $ grid .~ opt^.grid
    $ def,
    opacity .~ opt^.opacity
    $ cols .~ [opt^.col]
    $ align .~ BarsLeft
    $ space .~ 0
    $ def)


hist_ ∷ F.Foldable f ⇒ HistOption → f Double → EitherLayout
hist_ opt xs' = mapped.layout_x_axis.laxis_generate .~ const xAxis $ plot_ popt [bs]
    where
        (popt, bopt) = convertOpt opt
        bs = bars bopt (Just labels', [counts])

        xs = F.toList xs'
        numBins = (opt^.breaks) xs
        labels' = autoSteps numBins xs
        counts = V.toList $ histogram_ (length labels' - 1) (minimum labels') (maximum labels') (V.fromList xs)

        xAxis ∷ AxisData Double
        xAxis = AxisData {
            _axis_visibility = def,
            _axis_viewport = vmap (min', max'),
            _axis_tropweiv = invmap (min', max'),
            _axis_ticks = [ (v,-5) | v ← newLabels ],
            _axis_grid = labels',
            _axis_labels = [[ (v, show v) | v ← newLabels ]]
            }
                where
                    newLabels = let k = ceiling ((fromIntegral (length labels') / 5) ∷ Double)
                                    l = filter (\(i,_) → (i `mod` k) == 0) $ zip [(1∷Int)..] labels'
                                 in snd $ unzip l
                    halfW = (labels'!!1 - head labels') / 2
                    min' = minimum labels' - halfW
                    max' = maximum labels' + halfW

hist ∷ F.Foldable f ⇒ HistOption → f Double → IO ()
hist opt xs = either f f (hist_ opt xs)
    where f x = renderableToWindow (toRenderable x) (opt^.width) (opt^.height)

hist' ∷ F.Foldable f ⇒ HistOption → f Double → String → IO (PickFn ())
hist' opt xs = either f f (hist_ opt xs)
    where 
        f x = renderableToFile
            (fo_size .~ (opt^.width, opt^.height) $ def)
            (toRenderable x)
