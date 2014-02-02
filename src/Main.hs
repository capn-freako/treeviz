{-# LANGUAGE CPP, TemplateHaskell, InstanceSigs #-}
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

import Control.Monad (unless, forM_, liftM2)
import Data.List (stripPrefix, elem)
import Data.Complex
import System.Exit (exitFailure)
import System.Random (randoms, randomRs, getStdRandom)
import Test.QuickCheck (choose, vectorOf, elements, collect)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.All (quickCheckAll)
import Data.Tree (drawTree, levels, flatten)
import Data.Tree.LogTree (buildTree, dotLogTree, newFFTTree
                        , getLevels, getFlatten, getEval, newTreeData)
import Data.Newtypes.PrettyDouble (PrettyDouble(..))

-- Global constants
treeFileName   = "tree.gv"
legendFileName = "legend.gv"

-- Determines the prime factors of an integer.
primeFactors :: Int -> [Int]
primeFactors n
    | isPrime n = [n]
    | otherwise = primeDivisor : primeFactors result
        where result       = n `div` primeDivisor
              primeDivisor = head $ filter ((== 0) . (n `mod`)) primes

-- Tests an integer for primeness.
isPrime :: Int -> Bool
isPrime n = elem n $ takeWhile (<= n) primes

-- Prime generator. (Sieve of Eratosthenes)
primes :: [Int]
primes = primesRecurse [2..]

primesRecurse :: [Int] -> [Int]
primesRecurse ns = n : primesRecurse ms
    where n  = head ns
          ms = filter ((/= 0) . (`mod` n)) ns

-- Discrete Fourier Transform (DFT)
-- O(n^2)
--
dft :: RealFloat a => [Complex a] -> [Complex a]
dft xs = [ sum [ x * exp((0.0 :+ (-1.0)) * 2 * pi / lenXs * fromIntegral(k * n))
                 | (x, n) <- zip xs [0..]
               ]
           | k <- [0..(length xs - 1)]
         ]
    where lenXs = fromIntegral $ length xs

-- QuickCheck types & propositions
newtype FFTTestVal = FFTTestVal {
    getVal :: ([Complex PrettyDouble], [(Int, Bool)])
} deriving (Show)
instance Arbitrary FFTTestVal where
    arbitrary = do
        n      <- choose (2, 100)
        let radices = primeFactors n
        rValues <- vectorOf n $ choose (-1.0, 1.0)
        let values = map ((:+ PrettyDouble 0.0) . PrettyDouble) rValues
        -- This doesnt work, although I think it should; why not?:
--        difs   <- vectorOf (length radices) $ elements [True, False]
        dif <- elements [True, False]
        let difs = replicate (length radices) dif
        return $ FFTTestVal (values, zip radices difs)

prop_fft_test testVal = collect (length values) $ collect modes $
    (getEval $ buildTree newFFTTree tData) == answer
    where types  = testVal :: FFTTestVal
          tData  = newTreeData modes values
          modes  = snd $ getVal testVal
          values = fst $ getVal testVal
          answer = dft values

--- Test vectors
--tData1  = newTreeData [(2, False), (2, False), (2, False)] [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
tData1  = newTreeData [(2, True), (2, True), (2, False)] [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
answer1 = [8.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
tData2  = newTreeData [(2, True), (2, True), (2, False)] [1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
answer2 = [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
tData3  = newTreeData [(2, False), (2, True), (2, False)] [1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0]
answer3 = [0.0, 0.0, 0.0, 0.0, 8.0, 0.0, 0.0, 0.0]
tData4  = newTreeData [(2, False), (2, False), (2, False)] [0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0]
answer4 = [1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0]
tData5  = newTreeData [(2, False), (2, False), (2, False)] [3.000651293151413e-2 :+ 0.0,(-0.40689552869429835) :+ 0.0,(-7.699660989251922e-2) :+ 0.0,0.5722622492704406 :+ 0.0,0.9132167229339474 :+ 0.0,0.7459175605048463 :+ 0.0,0.3074622261846285 :+ 0.0,(-0.9358905129416841) :+ 0.0]
tData5Values  = [3.000651293151413e-2 :+ 0.0,(-0.40689552869429835) :+ 0.0,(-7.699660989251922e-2) :+ 0.0,0.5722622492704406 :+ 0.0,0.9132167229339474 :+ 0.0,0.7459175605048463 :+ 0.0,0.3074622261846285 :+ 0.0,(-0.9358905129416841) :+ 0.0] :: [Complex PrettyDouble]
answer5 = [1.149 :+ 0.000, (-2.765) :+ 0.133, 0.713 :+ (-0.703), 0.998 :+ (-0.636), 1.198 :+ 0.000, 0.998 :+ 0.636, 0.713 :+ 0.703, (-2.765) :+ (-0.133)]
tData6  = newTreeData [(2, False), (5, False)]
                      [1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0]
tData7  = newTreeData [(2, False), (2, False)]
                      [1.0, 1.0, 1.0, 1.0]
tData8  = newTreeData [(2, True), (2, False)]
                      [1.0, 1.0, 1.0, 1.0]
tData9  = newTreeData [(2, False), (2, True), (2, False)] [1.0, 1.0, -1.0, -1.0, 1.0, 1.0, -1.0, -1.0]
answer9 = [0.0, 0.0, 4.0 :+ (-4.0), 0.0, 0.0, 0.0, 4.0 :+ 4.0, 0.0]

-- Main
exeMain = do
    let tData  = tData5
    let answer = answer5
    let tree   = buildTree newFFTTree tData
    let res    = getEval tree
    putStrLn $ "Result = \t\t" ++ show res
    if  answer == res
      then putStrLn "Pass."
      else putStrLn $ "Fail.\nAnswer = \t" ++ show answer
    let (treePlot, legendPlot) = dotLogTree tree
    writeFile treeFileName   treePlot
    writeFile legendFileName legendPlot

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
