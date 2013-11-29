{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  Copyright (c) 2013 David Banas; all rights reserved World wide.
-- License     :  AllRightsReserved
--
-- Maintainer  :  capn.freako@gmail.com
-- Stability   :  Experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Control.Monad (unless, forM_)
import Data.List (stripPrefix)
import Data.Complex
import System.Exit (exitFailure)
import Test.QuickCheck.All (quickCheckAll)
import Data.Tree(drawTree, levels, flatten)
import Data.Tree.LogTree( buildTree, dotLogTree, newFFTTree
                        , getLevels, getFlatten, getEval, newTreeData)

-- Global constants
kDotFileName = "test.dot"

-- Test vectors
tData_1  = newTreeData [(2, False), (2, False), (2, False)] [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
answer_1 = [8.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
tData_2  = newTreeData [(2, True), (2, True), (2, True)] [1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
answer_2 = [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
tData_3  = newTreeData [(2, True), (2, True), (2, True)] [1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0]
answer_3 = [0.0, 0.0, 0.0, 0.0, 8.0, 0.0, 0.0, 0.0]
tData_4  = newTreeData [(2, False), (2, False), (2, False)] [0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0]
answer_4 = [1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0]

-- Determines whether two floating point vectors are "equal".
floatMatch :: [Complex Float] -> [Complex Float] -> Bool
floatMatch []     []     = True
floatMatch []     (y:ys) = False
floatMatch (x:xs) []     = False
floatMatch xs     ys     = error < (0.0001 * signal) -- .01% or -40dB
    where error  = sum $ map ((** 2) . realPart . abs) [y - x | (x, y) <- zip xs ys]
          signal = sum $ map ((** 2) . realPart . abs) xs

-- QuickCheck propositions
prop_fft_test_1 = floatMatch (getEval $ buildTree newFFTTree tData_1)
                             answer_1
prop_fft_test_2 = floatMatch (getEval $ buildTree newFFTTree tData_2)
                             answer_2
prop_fft_test_3 = floatMatch (getEval $ buildTree newFFTTree tData_3)
                             answer_3
prop_fft_test_4 = floatMatch (getEval $ buildTree newFFTTree tData_4)
                             answer_4

-- Main
exeMain = do
    let tData  = tData_4
    let tree   = buildTree newFFTTree tData
    let res    = getEval tree
    writeFile kDotFileName $ dotLogTree tree

-- Entry point for unit tests.
testMain = do
    allPass <- $quickCheckAll -- Run QuickCheck on all prop_ functions
    unless allPass exitFailure

-- This is a clunky, but portable, way to use the same Main module file
-- for both an application and for unit tests.
-- MAIN_FUNCTION is preprocessor macro set to exeMain or testMain.
-- That way we can use the same file for both an application and for tests.
#ifndef MAIN_FUNCTION
#define MAIN_FUNCTION exeMain
#endif
main = MAIN_FUNCTION
