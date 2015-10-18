{-# LANGUAGE GeneralizedNewtypeDeriving
  #-}

{-|
Module      :  Data.Newtypes.PrettyDouble
Description :  A custom double for printing nice numbers in plots.
Copyright   :  Copyright (c) 2013 David Banas; all rights reserved World wide.
License     :  BSD3

Maintainer  :  capn.freako@gmail.com
Stability   :  Development
Portability :
-}
module Data.Newtypes.PrettyDouble (
    PrettyDouble (..)
) where

import Text.Printf

{-|
A custom double precision floating point type, which overides the 'Show'
and 'Eq' instances, in order to:

    * Limit precision to 3 places after the decimal.

    * Print ridiculously small numbers as simply zero.

    * Define equality as difference being less than some threshold.
-}
newtype PrettyDouble = PrettyDouble {
    uglyDouble :: Double
  } deriving (Ord, Fractional, Floating, Real, RealFrac, RealFloat, Enum)

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
              
instance Num PrettyDouble where
    x + y         = PrettyDouble $ uglyDouble x + uglyDouble y
    x - y         = PrettyDouble $ uglyDouble x - uglyDouble y
    x * y         = PrettyDouble $ uglyDouble x * uglyDouble y
    abs x         = PrettyDouble $ abs $ uglyDouble x
    signum x      = PrettyDouble $ signum $ uglyDouble x
    fromInteger n = PrettyDouble $ fromIntegral n

