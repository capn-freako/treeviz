{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}

-----------------------------------------------------------------------------
--
-- Module      :  Data.Tree.LogTree
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

module Data.Tree.LogTree (
    radix2DITTree, showLogTree, getLevels, getFlatten, doDrawTree
) where

import Data.Complex
import Data.Tree

-- radix2_DIT - Calculates the FFT, making the radix-2 decimation-in-time
--              algorithm explicit.
--
-- This function probably won't be used much, given the general intent of
-- this module. It was origiinally written, for two purposes:
--   1) To ensure I had the algorithm correctly understood.
--   2) To provide a reference for constructing the `radix2_DIT_Tree'
--      function, which is the real meat of the act, here.
radix2_DIT :: RealFloat a => Bool -> [Complex a] -> [Complex a]
radix2_DIT _ []    = []
radix2_DIT _ [x]   = [x]
radix2_DIT rev xs  = (++) (zipWith (+) xes xos) (zipWith (-) xes xos)
    where wn | rev       = exp (0.0 :+ ( 2.0 * pi / (fromIntegral (length xs))))
             | otherwise = exp (0.0 :+ (-2.0 * pi / (fromIntegral (length xs))))
          xes            = radix2_DIT rev (evens xs)
          xos            = zipWith (*) (radix2_DIT rev (odds xs))
                                       [wn ** (fromIntegral k) | k <- [0..]]

evens :: [a] -> [a]
evens []     = []
evens [x]    = [x]
evens [x, y] = [x]
evens (x:xs) = x : evens (tail xs)

odds :: [a] -> [a]
odds []        = []
odds [x]       = []
odds [x, y]    = [y]
odds [x, y, z] = [y]
odds (x:xs)    = head xs : odds (tail xs)

-- Data.Tree.LogTree - a tree structure representing logarithmic decomposition
--                     of arbitrary radix and using either decimation-in-time
--                     (DIT), or decimation-in-frequency (DIF) approach.
--
--   a       = data type of original list elements.
--
--   Tree type tuple, from left to right:
--     a     = original list element value, for leaf; None, otherwise.
--     [Int] = starting indeces, in original input list, of children
--     Int   = index skip factor, in original input list, of children
--
-- Notes)
--   1) The radix of the decomposition represented by any tree is equal
--      to the number of children at any node (i.e. - length subForest),
--      which is also equal to the length of the second element in the
--      Tree type tuple.
--   2) A value >1 in the third element of the Tree type tuple indicates
--      decimation-in-time (DIT) was used to decompose the original list.
--      Otherwise, decimation-in-frequency (DIF) was used.
type LogTree a = Tree (Maybe a, [Int], Maybe Int)

-- radix2DITTree Takes a list of values and constructs the tree representing
--               the radix-2, decimation-in-time (DIT) decomposition of the
--               list for processing.
radix2DITTree :: [a] -> Either String (LogTree a)
radix2DITTree []     = Left "Error: radix2DITTree() called with empty list."
radix2DITTree [x]    = Left "Error: radix2DITTree() called with singleton list."
radix2DITTree xs     = do
    l <- radix2DITSubtree 0 2 (evens xs)
    r <- radix2DITSubtree 1 2 (odds  xs)
    return $ Node (Nothing, [0, 1], Just 2) [l, r]

-- radix2DITSubtree This "recursion helper" is necessary, since we only need
--                  the extra information it carries along, after the first
--                  call to radix2DITTree. That is, we don't want to burden
--                  the user with having to supply bogus values for the
--                  `offset' & `skip' parameters, in the call to radix2DITTree.
radix2DITSubtree :: Int -> Int -> [a] -> Either String (LogTree a)
radix2DITSubtree _ _ []  = Left "Error: radix2DITSubtree() called with empty list."
radix2DITSubtree _ _ [x] = Left "Error: radix2DITSubtree() called with singleton list."
radix2DITSubtree offset skip [x, y] =
    do return $
        Node (Nothing, [offset, offset + skip], Nothing) [
            Node (Just x, [], Nothing) []
          , Node (Just y, [], Nothing) []
          ]
radix2DITSubtree offset skip xs     = do
    l <- radix2DITSubtree  offset         (2 * skip) (evens xs)
    r <- radix2DITSubtree (offset + skip) (2 * skip) (odds  xs)
    return $ Node (Nothing, [offset, offset + skip], Just (2 * skip)) [l, r]

-- showLogTree converts a LogTree to a string representation.
--
-- Example)
--   showLogTree $ radix2DITTree [...]
showLogTree :: (Show a) => Either String (LogTree a) -> String
showLogTree (Left msg)   = msg
showLogTree (Right tree) = showLogTreeRecurse "" tree

showLogTreeRecurse :: (Show a) => String -> LogTree a -> String
showLogTreeRecurse _      (Node (Just x,  [],                   Nothing) [])     = -- leaf
    show x
showLogTreeRecurse indent (Node (Nothing, [x_offset, y_offset], Nothing) [x, y]) = -- penultimate node
    indent
    ++ "["
    ++ (show x_offset) ++ "(" ++ (show $ getValue x) ++ ")" ++ " + "
    ++ (show y_offset) ++ "(" ++ (show $ getValue y) ++ ")"
    ++ ", "
    ++ (show x_offset) ++ "(" ++ (show $ getValue x) ++ ")" ++ " - "
    ++ (show y_offset) ++ "(" ++ (show $ getValue y) ++ ")"
    ++ "]"
showLogTreeRecurse indent (Node (Nothing, [l_offset, r_offset], Just skip) [l, r]) = -- ordinary node
    indent
    ++ concat [ "(" ++ "\n"
             ++ showLogTreeRecurse (indent ++ "  ") l ++ "(" ++ show k ++ ")" ++ "+"
             ++ "W(" ++ show (2 * (num_elems)) ++ ", " ++ show k ++ ")" ++ " * "
             ++ showLogTreeRecurse (indent ++ "  ") r ++ "(" ++ show k ++ ")"
             ++ "), "
         | k <- [0..(num_elems)]
       ]
    ++ concat [ "(" ++ "\n"
             ++ showLogTreeRecurse (indent ++ "  ") l ++ "(" ++ show k ++ ")" ++ "-"
             ++ "W(" ++ show (2 * (num_elems)) ++ ", " ++ show k ++ ")" ++ " * "
             ++ showLogTreeRecurse (indent ++ "  ") r ++ "(" ++ show k ++ ")"
             ++ "), "
         | k <- [0..(num_elems)]
       ]
    where num_elems = length $ flatten l

-- dotLogTree converts a LogTree to a GraphViz dot diagram.
--
-- Example)
--   dotLogTree $ radix2DITTree [...]
dotLogTree :: (Show a) => Either String (LogTree a) -> String
dotLogTree (Left msg)   = msg
dotLogTree (Right tree) =
    "digraph g { \n \
        graph [ \n \
            rankdir = \"RL\" \n \
        ]; \n \
        node [ \n \
            fontsize = \"16\" \n \
            shape = \"ellipse\" \n \
        ]; \n \
        edge [ \n \
        ];\n"
  ++ dotLogTreeRecurse tree
  ++ "}\n"

dotLogTreeRecurse :: (Show a) => LogTree a -> String
dotLogTreeRecurse (Node (Just x,  [],                   Nothing) [])     = -- leaf
    show x
dotLogTreeRecurse (Node (Nothing, [x_offset, y_offset], Nothing) [x, y]) = -- penultimate node
    map
    "\"node0\" [label = \"<f0> 0x10ba8| <f1>\" shape = \"record\"];"
    ++ "["
    ++ (show x_offset) ++ "(" ++ (show $ getValue x) ++ ")" ++ " + "
    ++ (show y_offset) ++ "(" ++ (show $ getValue y) ++ ")"
    ++ ", "
    ++ (show x_offset) ++ "(" ++ (show $ getValue x) ++ ")" ++ " - "
    ++ (show y_offset) ++ "(" ++ (show $ getValue y) ++ ")"
    ++ "]"
dotLogTreeRecurse indent (Node (Nothing, [l_offset, r_offset], Just skip) [l, r]) = -- ordinary node
    indent
    ++ concat [ "(" ++ "\n"
             ++ dotLogTreeRecurse (indent ++ "  ") l ++ "(" ++ show k ++ ")" ++ "+"
             ++ "W(" ++ show (2 * (num_elems)) ++ ", " ++ show k ++ ")" ++ " * "
             ++ dotLogTreeRecurse (indent ++ "  ") r ++ "(" ++ show k ++ ")"
             ++ "), "
         | k <- [0..(num_elems)]
       ]
    ++ concat [ "(" ++ "\n"
             ++ dotLogTreeRecurse (indent ++ "  ") l ++ "(" ++ show k ++ ")" ++ "-"
             ++ "W(" ++ show (2 * (num_elems)) ++ ", " ++ show k ++ ")" ++ " * "
             ++ dotLogTreeRecurse (indent ++ "  ") r ++ "(" ++ show k ++ ")"
             ++ "), "
         | k <- [0..(num_elems)]
       ]
    where num_elems = length $ flatten l

getValue :: LogTree a -> Maybe a
getValue (Node (x, _, _) _) = x

-- These helper functions just unwrap the Either from arround a
-- LogTree, so that the equivalent functions from Data.Tree can be used.
getLevels (Left msg)   = [] -- Can't figure out how to usefully cary `msg' forward.
getLevels (Right tree) = levels tree

getFlatten (Left msg)   = [] -- (as above)
getFlatten (Right tree) = levels tree

doDrawTree (Left msg)   = msg
doDrawTree (Right tree) = drawTree tree
