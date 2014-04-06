{-# LANGUAGE OverloadedStrings, UnicodeSyntax, BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Graphics.Rendering.HPlot.Types (
      PlotOption
    , BarOption
    , LineOption
    , PointOption
    , HistOption
    , HeatMapOption

    -- | Lens
    , title
    , labels
    , xlab
    , ylab
    , xlim
    , ylim
    , width
    , height
    , grid
    , cols
    , opacity
    , align
    , space
    , style
    , legend
    , col
    , lty
    , lwd
    , radius
    , shape
    , breaks
    , labRow
    , labCol
    , palette

    , with
    , (.~)
    , (&)

    , EitherPlot
    , EitherLayout
    ) where

import Graphics.Rendering.Chart
import Graphics.Rendering.HPlot.Utils
import Data.Colour
import Data.Colour.SRGB
import Control.Lens
import Data.Default

with ∷ Default a ⇒ a
with = def

data PlotOption = PlotOption {
    _plotTitle ∷ String
    , _plotLabels ∷ [String]
    , _plotXlab ∷ String
    , _plotYlab ∷ String
    , _plotXlim ∷ (Double, Double)
    , _plotYlim ∷ (Double, Double)
    , _plotWidth ∷ Int
    , _plotHeight ∷ Int
    , _plotGrid ∷ Char
    } deriving (Show)

makeFields ''PlotOption

instance Default PlotOption where
    def = PlotOption {
        _plotTitle = []
        , _plotLabels = []
        , _plotXlab = []
        , _plotYlab = []
        , _plotXlim = (0, -1)
        , _plotYlim = (0, -1)
        , _plotWidth = 480
        , _plotHeight = 480
        , _plotGrid = 'b'
    }

data BarOption = BarOption {
    _barCols ∷ [String]
    , _barOpacity ∷ Double
    , _barAlign ∷ PlotBarsAlignment
    , _barSpace ∷ Double
    , _barStyle ∷ PlotBarsStyle
    , _barLegend ∷ [String]
    }

makeFields ''BarOption

instance Default BarOption where
    def = BarOption {
        _barOpacity = 1.0
        , _barCols = ["blue", "red", "green", "yellow", "cyan", "magenta"]
        , _barAlign = BarsCentered
        , _barSpace = 15
        , _barStyle = BarsClustered
        , _barLegend = []
    }

data LineOption = LineOption {
      _lineCol ∷ String
    , _lineOpacity ∷ Double
    , _lineLty ∷ Int
    , _lineLwd ∷ Double
    , lineJoin ∷ LineJoin
    }

makeFields ''LineOption

instance Default LineOption where
    def = LineOption {
          _lineOpacity = 1.0
        , _lineCol = "blue"
        , _lineLty = 1
        , _lineLwd = 1
        , lineJoin = LineJoinMiter
    }

data PointOption = PointOption {
    _pointRadius ∷ Double
    , _pointShape ∷ Char
    , _pointCol ∷ String
    , _pointOpacity ∷ Double
    , _pointLwd ∷ Double
    }

makeFields ''PointOption

instance Default PointOption where
    def = PointOption {
          _pointRadius = 3
        , _pointShape = '.'
        , _pointLwd = 1
        , _pointOpacity = 1.0
        , _pointCol = "blue"
    }

data HeatMapOption = HeatMapOption {
      _heatmapLabRow ∷ [String]
    , _heatmapLabCol ∷ [String]
    , _heatmapSpace ∷ Double
    , _heatmapTitle ∷ String
    , _heatmapXlab ∷ String
    , _heatmapYlab ∷ String
    , _heatmapWidth ∷ Int
    , _heatmapHeight ∷ Int
    , _heatmapPalette ∷ [Colour Double]
    , _heatmapOpacity ∷ Double
}

makeFields ''HeatMapOption

instance Default HeatMapOption where
    def = HeatMapOption {
          _heatmapLabRow = []
        , _heatmapLabCol = []
        , _heatmapSpace = 1
        , _heatmapTitle = ""
        , _heatmapXlab = ""
        , _heatmapYlab = ""
        , _heatmapWidth = 480
        , _heatmapHeight = 480
        , _heatmapPalette = warmCols
        , _heatmapOpacity = 1
    }

-- | print safe
warmCols ∷ [Colour Double]
warmCols = [
    sRGB24 255 255 178,
    sRGB24 254 217 118,
    sRGB24 254 178 76,
    sRGB24 253 141 60,
    sRGB24 240 59 32,
    sRGB24 189 0 38
    ]

coolCols ∷ [Colour Double]
coolCols = [
    sRGB24 255 255 204,
    sRGB24 199 233 180,
    sRGB24 127 205 187,
    sRGB24 65 182 196,
    sRGB24 44 127 184,
    sRGB24 37 52 148
    ]

type BreakRule = [Double] → Int

data HistOption = HistOption {
    _histTitle ∷ String
    , _histXlab ∷ String
    , _histYlab ∷ String
    , _histWidth ∷ Int
    , _histHeight ∷ Int
    , _histXlim ∷ (Double, Double)
    , _histYlim ∷ (Double, Double)
    , _histGrid ∷ Char
    , _histCol ∷ String
    , _histOpacity ∷ Double
    , _histBreaks ∷ BreakRule
    }

makeFields ''HistOption

instance Default HistOption where
    def = HistOption {
        _histTitle = []
        , _histXlab = []
        , _histYlab = "Frequency"
        , _histWidth = 480
        , _histHeight = 480
        , _histXlim = (0, -1)
        , _histYlim = (0, -1)
        , _histGrid = 'y'
        , _histCol = "blue"
        , _histOpacity = 1.0
        , _histBreaks = freedmanDiaconis
    }

type EitherPlot = Either (Plot PlotIndex Double) (Plot Double Double)

type EitherLayout = Either (Layout PlotIndex Double) (Layout Double Double)
