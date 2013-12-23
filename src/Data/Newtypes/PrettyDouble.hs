{-# LANGUAGE GeneralizedNewtypeDeriving
  #-}

-----------------------------------------------------------------------------
--
-- Module      :  Data.Newtypes.PrettyDouble
-- Copyright   :  Copyright (c) 2013 David Banas; all rights reserved World wide.
-- License     :  BSD3
--
-- Maintainer  :  capn.freako@gmail.com
-- Stability   :  Experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Data.Newtypes.PrettyDouble (
    PrettyDouble (..)
) where

import Text.Printf

newtype PrettyDouble = PrettyDouble {
    value :: Double
  } deriving (Num, Eq, Ord, Fractional, Floating, Real, RealFrac, RealFloat)
instance Show PrettyDouble where
    show = printf "%10.3g" . zeroThresh . value
        where zeroThresh y =
                if abs y < 1.0e-10
                then 0.0
                else y
