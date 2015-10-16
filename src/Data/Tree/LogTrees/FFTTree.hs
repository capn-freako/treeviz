{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , TypeFamilies
           , Rank2Types
  #-}

{-|
Module      :  Data.Tree.LogTrees.FFTTree
Description :  An instance of typeclass 'LogTree', which represents a
               Fast Fourier Transform (FFT) of arbitrary radices and
               decimation styles.
Copyright   :  Copyright (c) 2014 David Banas; all rights reserved World wide.
License     :  BSD3

Maintainer  :  capn.freako@gmail.com
Stability   :  Development
Portability :
-}

module Data.Tree.LogTrees.FFTTree (
    FFTTree (..), newFFTTree
) where

import Data.Complex
import Data.Tree
import Data.List
import Data.Newtypes.PrettyDouble (PrettyDouble(..))
import Data.Tree.LogTree

-- | An instance of LogTree, this type represents the Fast Fourier
--   Transform (FFT) of arbitrary radix and decimation scheme.
type FFTTree = GenericLogTree (Complex PrettyDouble)
instance LogTree FFTTree (Complex PrettyDouble) where
    -- The evaluation of a leaf is its value, multiplied by the correct series of accumulated twiddles.
    evalNode (Node (Just (x, wss),  _, _,   _)        _, ms) = [foldl (*) x (zipWith getItem ms wss)]
      where getItem m ws = ws !! m
    -- Sub-trees are evaluated recursively, but require inclusion of `phasors'.
    evalNode (Node (            _,  _, _, decimationType) children, ms) =
        foldl (zipWith (+)) [0.0 | n <- [1..nodeLen]]
          $ zipWith (zipWith (*)) subTransforms phasors
      where subTransforms =
                [ subCombFunc
                    $ map evalNode
                          [(child, ms ++ [m]) | m <- [0..(radix - 1)]]
                  | child <- children
                ]
            subCombFunc =
              if decimationType == DIF then concat . transpose -- i.e. - interleave
                     else concat
            childLen = length $ last(levels $ head children)
            radix    = length children
            nodeLen  = childLen * radix
            phasors  = [ [ cis((-2.0) * pi / degree * fromIntegral r * fromIntegral k)
                           | k <- [0..(nodeLen - 1)]]
                         | r <- [0..(radix - 1)]]
            degree   | decimationType == DIF = fromIntegral radix
                     | otherwise = fromIntegral nodeLen

    getTwiddles (Node (     _,  _, _, decimationType) children) = calcTwiddles decimationType childLen radix
        where childLen = length $ last(levels $ head children)
              radix    = length children

    calcTwiddles decimationType childLen radix =
        if decimationType == DIF
          then [ [ cis((-2.0) * pi / fromIntegral nodeLen * fromIntegral m * fromIntegral n)
                   | n <- [0..(childLen - 1)]]
                 | m <- [0..(radix - 1)]]
          else [ [ 1.0 :+ 0.0
                   | n <- [0..(childLen - 1)]]
                 | m <- [0..(radix - 1)]]
        where nodeLen  = childLen * radix

    getTwiddleStrs (Node (     _,  _, _, decimationType) children) =
        if decimationType == DIF
          then map (map ((\str -> " [" ++ str ++ "]") . show)) $ getTwiddles (Node (Nothing, [], 0, decimationType) children)
          else [["" | i <- [1..(length (last (levels child)))]] | child <- children]

    getCompNodes (Node ( Just x, _, _,   _)        _) = [] -- A leaf requires no computational nodes.
    getCompNodes (Node (Nothing, _, _, decimationType) children) =
        [ [ (Sum, [ cis (-2.0 * pi * k * r / degree)
                    | r <- map fromIntegral [0..(radix - 1)]
                  ]
            )
            | k <- map fromIntegral [childLen * r + m | r <- [0..(radix - 1)]]
          ]
          | m <- map fromIntegral [0..(childLen - 1)]
        ] where childLen = fromIntegral $ length $ last(levels $ head children)
                radix    = length children
                nodeLen  = childLen * radix
                degree   | decimationType == DIF = fromIntegral radix
                         | otherwise = fromIntegral nodeLen

--    getAllCompNodes t = getCompNodes t
--                     ++ (concatMap getAllCompNodes (subForest t))

-- | Returns a tree builder suitable for constructing Fast Fourier Transform
--   (FFT) decomposition trees of arbitrary radices and either decimation
--   style (i.e. - DIT or DIF).
newFFTTree :: TreeBuilder FFTTree
newFFTTree = TreeBuilder buildMixedRadixTree
