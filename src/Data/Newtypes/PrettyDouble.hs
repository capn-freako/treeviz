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
    uglyDouble :: Double
  } deriving (Num, Ord, Fractional, Floating, Real, RealFrac, RealFloat)
instance Show PrettyDouble where
    show = printf "%6.3g" . zeroThresh . uglyDouble
        where zeroThresh y =
                if abs y < 1.0e-10
                then 0.0
                else y

instance Eq PrettyDouble where
    z1' == z2' = abs (z1 - z2) < 1.0e-3
        where z1 = uglyDouble z1'
              z2 = uglyDouble z2'

