{-# LANGUAGE StandaloneDeriving
           , FlexibleContexts
           , UndecidableInstances
           , ParallelListComp #-}

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
    radix2DITTree, dotLogTree, getLevels, getFlatten
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

-- dotLogTree converts a LogTree to a GraphViz dot diagram.
--
-- Example)
--   dotLogTree $ radix2DITTree [...]
dotLogTree :: Either String (LogTree (Complex Float)) -> String
dotLogTree (Left msg)   = header
 ++ "\"node0\" [label = \"" ++ msg ++ "\"]\n"
 ++ "}\n"
dotLogTree (Right tree) = header
 ++ dotLogTreeRecurse "0" tree
 ++ "}\n"

header = "digraph g { \n \
 \   graph [ \n \
 \       rankdir = \"RL\" \n \
 \       splines = \"false\" \n \
 \   ]; \n \
 \   node [ \n \
 \       fontsize = \"16\" \n \
 \       shape = \"ellipse\" \n \
 \       height = \"0.3\" \n \
 \   ]; \n \
 \   edge [ \n \
 \   ];\n \
 \   ranksep = \"1.5\" \
 \   nodesep = \"0\""

dotLogTreeRecurse :: String -> LogTree (Complex Float) -> String
dotLogTreeRecurse nodeID (Node (Just x,            _,    _)      _) = -- leaf
    -- Draw myself.
    "\"node" ++ nodeID ++ "\" [label = \"<f0> "
    ++ (show x)
    ++ "\" shape = \"record\"];\n"
dotLogTreeRecurse nodeID (Node (     _, [loff, roff], skip) [l, r]) = -- ordinary node
    -- Draw myself.
    "\"node" ++ nodeID ++ "\" [label = \"<f0> " ++ (show (head res))
    ++ (concat [" | <f" ++ (show k) ++ "> " ++ (show val)
                 | (val, k) <- zip (tail res) [1..]])
    ++ "\" shape = \"record\"];\n"
    -- Draw children.
    ++ (dotLogTreeRecurse lID l)
    ++ (dotLogTreeRecurse rID r)
    -- Draw my connections to my children.
    ++ (unlines [ "\"node" ++ nodeID ++ "\":f" ++ (show k) ++ " -> \"node"
                  ++ lID ++ "\":f" ++ (show (k `mod` num_child_elems))
                  ++ ":e"
                  ++ " [id = \"" ++ nodeID ++ lID ++ (show k) ++ "\""
                  ++ ", decorate = \"true\""
                  ++ ", dir = \"back\"];\n"
                  ++ "\"node" ++ nodeID ++ "\":f" ++ (show k) ++ " -> \"node"
                  ++ rID ++ "\":f" ++ (show (k `mod` num_child_elems))
                  ++ ":e"
                  ++ " [id = \"" ++ nodeID ++ rID ++ (show k) ++ "\""
                  ++ ", sametail = \"" ++ nodeID ++ lID ++ (show k) ++ "\""
                  ++ ", decorate = \"true\""
                  ++ ", dir = \"back\""
                  ++ ", taillabel = " ++ opt_sign
                  ++ ", headlabel = " ++ twiddle
                  ++ "];\n"
                  |  k        <- [0..(num_elems - 1)]
                   , twiddle  <- do let res | k >= (num_child_elems) = "\"W(" ++ (show num_elems) ++ ", "
                                                                     ++ (show (k `mod` num_child_elems)) ++ ")\""
                                            | otherwise   = "\"\""
                                    return res
                   , opt_sign <- do let res | ((-1) ^ (k `div` num_child_elems)) < 0 = "\"(-)\""
                                            | otherwise                              = "\"\""
                                    return res
                ]
       )
    where num_elems       = 2 * num_child_elems
          num_child_elems = length $ head $ reverse $ levels l
          lID             = nodeID ++ "0"
          rID             = nodeID ++ "1"
          res             = evalNode $ Node (Nothing, [loff, roff], skip) [l, r]

-- evalNode - Evaluates a node in a tree, returning a list of values with
--            length equal to the sum of the lengths of the node's children.
evalNode :: LogTree (Complex Float) -> [Complex Float]
evalNode (Node (Just x,  _, _) [])     = [x]
evalNode (Node (Nothing, _, _) [l, r]) =
       zipWith (+) el tr
   ++ (zipWith (-) el tr)
    where el = evalNode l
          er = evalNode r
          tr = (zipWith (*) [exp((0.0 :+ (-1.0)) * ((pi * k / childLen) :+ 0.0))
                              | k <- map fromIntegral [0..]]
                            er)
          childLen = fromIntegral $ length $ head $ reverse $ levels l

-- Helper function to grab a node's value.
getValue :: LogTree a -> Maybe a
getValue (Node (x, _, _) _) = x

-- These helper functions just unwrap the Either from arround a
-- LogTree, so that the equivalent functions from Data.Tree can be used.
getLevels (Left msg)   = [] -- Can't figure out how to usefully cary `msg' forward.
getLevels (Right tree) = levels tree

getFlatten (Left msg)   = [] -- (as above)
getFlatten (Right tree) = levels tree
