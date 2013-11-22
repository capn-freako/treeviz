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
import System.Exit (exitFailure)
import Test.QuickCheck.All (quickCheckAll)
import Data.Tree(drawTree, levels, flatten)
import Data.Tree.LogTree( buildTree, dotLogTree, newFFTTree
                        , getLevels, getFlatten, getEval, newTreeData)

-- Global constants
kDotFileName = "test.dot"

-- Simple function to create a hello message.
hello s = "Hello " ++ s

-- Tell QuickCheck that if you strip "Hello " from the start of
-- hello s you will be left with s (for any s).
prop_hello s = stripPrefix "Hello " (hello s) == Just s

-- Main
exeMain = do
--    let tData  = newTreeData [(2, False), (2, False)] [1.0, 0.0, 0.0, 0.0]
--    let tData  = newTreeData [(2, True), (2, True), (2, True)] [1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
--    let tData  = newTreeData [(2, True), (2, True), (2, True)] [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
    let tData  = newTreeData [(2, False), (2, False), (2, False)] [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
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
