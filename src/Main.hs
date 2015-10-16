{-# LANGUAGE CPP, TemplateHaskell #-}

-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  Copyright (c) 2013 David Banas; all rights reserved World wide.
-- License     :  AllRightsReserved
--
-- Maintainer  :  capn.freako@gmail.com
-- Stability   :  Development
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Control.Monad (unless, forM, forM_, liftM2, liftM)
import Data.List (stripPrefix, elem)
import Data.Complex
import System.Exit (exitFailure)
import System.Random (randoms, randomRs, getStdRandom)
import Test.QuickCheck (choose, vectorOf, elements, collect)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.All (quickCheckAll)
import Data.Tree (drawTree, levels, flatten, subForest)
--import Data.Tree.LogTree (buildTree, newTreeData, DecompositionMode, Radix, DecimationType (..))
import Data.Tree.LogTree
import Data.Tree.LogTreeUtil (dotLogTree, getLevels, getFlatten, getEval)
import Data.Tree.LogTrees.FFTTree (FFTTree (..), newFFTTree)
import Data.Newtypes.PrettyDouble (PrettyDouble (..))

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
    getVal :: ([Complex PrettyDouble], [DecompositionMode])
} deriving (Show)
instance Arbitrary FFTTestVal where
    arbitrary = do
        n      <- choose (2, 100)
        let radices = primeFactors n
        rValues <- vectorOf n $ choose (-1.0, 1.0)
        let values = map ((:+ PrettyDouble 0.0) . PrettyDouble) rValues
        difs   <- vectorOf (length radices) $ elements [DIF, DIT]
        return $ FFTTestVal (values, zip radices difs)

prop_fft_test testVal = collect (length values) $ collect modes $
    (getEval $ buildTree newFFTTree tData) == answer
    where types  = testVal :: FFTTestVal
          tData  = newTreeData modes values
          modes  = snd $ getVal testVal
          values = fst $ getVal testVal
          answer = dft values

--- Test vectors
--tData1  = newTreeData [(2, DIT), (2, DIT), (2, DIT)] [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
tData1  = newTreeData [(2, DIF), (2, DIF), (2, DIT)] [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
answer1 = [8.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
tData2  = newTreeData [(2, DIF), (2, DIF), (2, DIT)] [1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
answer2 = [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
tData3  = newTreeData [(2, DIT), (2, DIF), (2, DIT)] [1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0]
answer3 = [0.0, 0.0, 0.0, 0.0, 8.0, 0.0, 0.0, 0.0]
tData4  = newTreeData [(2, DIT), (2, DIT), (2, DIT)] [0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0]
answer4 = [1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0]
tData5  = newTreeData [(2, DIT), (2, DIT), (2, DIT)] [3.000651293151413e-2 :+ 0.0,(-0.40689552869429835) :+ 0.0,(-7.699660989251922e-2) :+ 0.0,0.5722622492704406 :+ 0.0,0.9132167229339474 :+ 0.0,0.7459175605048463 :+ 0.0,0.3074622261846285 :+ 0.0,(-0.9358905129416841) :+ 0.0]
tData5Values  = [3.000651293151413e-2 :+ 0.0,(-0.40689552869429835) :+ 0.0,(-7.699660989251922e-2) :+ 0.0,0.5722622492704406 :+ 0.0,0.9132167229339474 :+ 0.0,0.7459175605048463 :+ 0.0,0.3074622261846285 :+ 0.0,(-0.9358905129416841) :+ 0.0] :: [Complex PrettyDouble]
answer5 = [1.149 :+ 0.000, (-2.765) :+ 0.133, 0.713 :+ (-0.703), 0.998 :+ (-0.636), 1.198 :+ 0.000, 0.998 :+ 0.636, 0.713 :+ 0.703, (-2.765) :+ (-0.133)]
tData6  = newTreeData [(2, DIT), (5, DIT)]
                      [1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0]
tData7  = newTreeData [(2, DIF), (2, DIF)]
                      [1.0, 0.0, 0.0, 0.0]
answer7 = [1.0, 1.0, 1.0, 1.0]
tData8  = newTreeData [(2, DIF), (2, DIF)]
                      [1.0, 1.0, 1.0, 1.0]
answer8 = [4.0, 0.0, 0.0, 0.0]
tData9  = newTreeData [(2, DIT), (2, DIF), (2, DIT)] [1.0, 1.0, -1.0, -1.0, 1.0, 1.0, -1.0, -1.0]
answer9 = [0.0, 0.0, 4.0 :+ (-4.0), 0.0, 0.0, 0.0, 4.0 :+ 4.0, 0.0]
tData10  = newTreeData [(2, DIT)]
                       [1.0, 0.0]
answer10 = [1.0, 1.0]

-- Main
exeMain = do
    -- Construct and verify the tree.
    let tData  = tData1
    let answer = answer1
    let tree   = buildTree newFFTTree tData
    case tree of
      Left msg -> putStrLn msg
      Right  t -> do
        -- Calculate, print, and check the FFT of the input data.
        let res = getEval tree
        putStrLn $ "Result = \t\t" ++ show res
        if  answer == res
          then putStrLn "Pass."
          else putStrLn $ "Fail.\nAnswer = \t" ++ show answer
        putStrLn ""

        -- Draw the computation flow graph.
        let (treePlot, legendPlot) = dotLogTree tree
        writeFile treeFileName   treePlot
        writeFile legendFileName legendPlot

        -- Print out twiddles.
        let items = last $ levels t
        twiddles <- forM items $ do
          \(item, _, _, _) -> do
            return $ liftM snd item
--        let twiddles = forM items $ \(item, _, _, _) -> liftM snd item
--        twiddles <- map (\(item, _, _, _) -> liftM snd item) items
        let trivial_twiddles = filter (== 1) twiddles
        putStrLn "Twiddles:"
        forM_ items $ do
          \(item, _, _, _) -> do
            putStrLn $ (show (liftM fst item)) ++ ":"
            putStrLn $ "\t" ++ (show (liftM snd item))
        putStrLn ""

        -- Print out the multiplication statistics.
        let compNodes = getAllCompNodes t
        let mults     = concat $ map snd $ concat compNodes
        let trivial_mults = filter (== 1) mults
        let non_trivial_mults = length twiddles - length trivial_twiddles + length mults - length trivial_mults
        putStrLn $ "Total computational nodes: " ++ (show $ length compNodes)
        putStrLn $ "Total multiplications: " ++ (show $ length mults)
        putStrLn $ "Trivial multiplications: " ++ (show $ length trivial_mults)
        putStrLn $ "Total twiddles: " ++ (show $ length twiddles)
        putStrLn $ "Trivial twiddles: " ++ (show $ length trivial_twiddles)
        putStrLn $ "Total non-trivial multiplications: " ++ (show non_trivial_mults)

-- Entry point for unit tests.
testMain = do
    allPass <- $quickCheckAll -- Run QuickCheck on all prop_ functions
    unless allPass exitFailure

testMain2 = go "sum-t3" (sum :: RTree N3 Int -> Int)
--main = go "FFTTree" (sum :: RTree N3 Int -> Int)

-- This is a clunky, but portable, way to use the same Main module file
-- for both an application and for unit tests.
-- MAIN_FUNCTION is preprocessor macro set to exeMain or testMain.
-- That way we can use the same file for both an application and for tests.
#ifndef MAIN_FUNCTION
#define MAIN_FUNCTION exeMain
#endif
main = MAIN_FUNCTION
