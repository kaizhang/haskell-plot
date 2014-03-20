{-# LANGUAGE OverloadedStrings, UnicodeSyntax, TemplateHaskell, BangPatterns #-}

module DataSets where

import Data.Vector

sample ∷ Vector Double
sample = fromList [1,2,3,5,6,8,19,2,3,56,2]
