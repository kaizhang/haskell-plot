{-# LANGUAGE OverloadedStrings, UnicodeSyntax, TemplateHaskell, BangPatterns #-}

import Type
import Hist
import Control.Lens
import DataSets
import Data.Default.Class

main = hist sample (
        common.title .~ "Test"
        $ common.xlab .~ "Test"
        $ def) "test1.png"
