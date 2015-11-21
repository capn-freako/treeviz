{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , TypeFamilies
           , Rank2Types
  #-}

{-|
Module      :  Data.Tree.LogTree2
Description :  A library for visualizing, evaluating, and implementing
               logarithmic breakdown of computation.
Copyright   :  Copyright (c) 2013 David Banas; all rights reserved World wide.
License     :  BSD3

Maintainer  :  capn.freako@gmail.com
Stability   :  Development
Portability :  Uncertain

Typical usage (a 4-point FFT, using 2 radix-2, DIT stages):

@
-- Main
exeMain = do
    let tData = newTreeData [(2, False), (2, False)] [1.0, 0.0, 0.0, 0.0]
    let tree  = buildTree newFFTTree tData
    case tree of
      Left msg -> putStrLn msg
      Right  _ -> do
        -- If you're interested in the numerical result:
        let res = getEval tree
        putStrLn $ "Result = \t\t" ++ show res
        -- If you want to visualize how the computation breaks down:
        let (treePlot, legendPlot) = dotLogTree tree
        writeFile treeFileName   treePlot
        writeFile legendFileName legendPlot
@

And, to get easily viewable *.PNGs from the two files written, above:

>>> dot <treeFileName> -Tpng >tree.png

>>> dot <legendFileName> -Tpng >legend.png

-}
module Data.Tree.LogTree2 (
    LogTree2 (..)
  , SizedVector (..)
  , svProd
  , svSum
) where

--import Data.Complex
--import Data.Tree
--import Data.List
--import Data.Newtypes.PrettyDouble (PrettyDouble(..))

-- | Enumerates the allowed input/output vector sizes.
--
--   (My hope is that we'll find a way to do away with this need of
--   explicit I/O vector size enumeration.)
data SizedVector a = Vector_2 a a              -- ^ A two element vector.
                   | Vector_4 a a a a          -- ^ A four element vector.
                   | Vector_8 a a a a a a a a  -- ^ An eight element vector.
    deriving (Eq, Show)

instance Functor SizedVector where
    fmap g (Vector_2 x0 x1) = Vector_2 (g x0) (g x1)
    fmap g (Vector_4 x0 x1 x2 x3) = Vector_4 (g x0) (g x1) (g x2) (g x3)
    fmap g (Vector_8 x0 x1 x2 x3 x4 x5 x6 x7) = Vector_8 (g x0) (g x1) (g x2) (g x3) (g x4) (g x5) (g x6) (g x7)

svProd :: Num a => SizedVector a -> SizedVector a -> SizedVector a
svProd (Vector_2 x0 x1) (Vector_2 y0 y1) = Vector_2 (x0 * y0) (x1 * y1)
svProd (Vector_4 x0 x1 x2 x3) (Vector_4 y0 y1 y2 y3) = Vector_4 (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3)
svProd (Vector_8 x0 x1 x2 x3 x4 x5 x6 x7) (Vector_8 y0 y1 y2 y3 y4 y5 y6 y7) = Vector_8 (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7)
svProd _ _ = undefined

svSum :: Num a => SizedVector a -> SizedVector a -> SizedVector a
svSum (Vector_2 x0 x1) (Vector_2 y0 y1) = Vector_2 (x0 + y0) (x1 + y1)
svSum (Vector_4 x0 x1 x2 x3) (Vector_4 y0 y1 y2 y3) = Vector_4 (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3)
svSum (Vector_8 x0 x1 x2 x3 x4 x5 x6 x7) (Vector_8 y0 y1 y2 y3 y4 y5 y6 y7) = Vector_8 (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7)
svSum _ _ = undefined

{-|
A class of tree structures representing logarithmic decomposition of arbitrary
radix and using either decimation-in-time (DIT), or decimation-in-frequency
(DIF) approach (or, a mix of both).
-}
class LogTree2 t a | t -> a where

    -- | Evaluates a tree, returning a computation from input to output
    --   vectors, where the length of the vectors is encoded in the vector type.
    evaluator :: LogTree2 t a => t -> SizedVector a -> SizedVector a

