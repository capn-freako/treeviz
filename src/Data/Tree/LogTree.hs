{-# LANGUAGE FlexibleContexts
           , UndecidableInstances
           , TypeSynonymInstances
           , FlexibleInstances
           , MultiParamTypeClasses
           , FunctionalDependencies
           , TypeFamilies
           , Rank2Types
           , GeneralizedNewtypeDeriving
  #-}

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
    newTreeData, dotLogTree
  , buildTree,   newFFTTree
  , getLevels,   getFlatten, getEval
  , modes,       values
  , PrettyDouble(..)
) where

import Data.Complex
import Data.Tree
import Data.List
import Text.Printf (printf, PrintfArg)
import Control.Monad.State.Lazy

-- Data.Tree.LogTree - a class of tree structures representing logarithmic
--                     decomposition of arbitrary radix and using either
--                     decimation-in-time (DIT), or
--                     decimation-in-frequency (DIF) approach.
--
--   a           = data type of original list elements.
--
--   Tree type tuple, from left to right:
--     Maybe a   = original list element value, for leaf; Nothing, otherwise.
--     [Int]     = starting indeces, in original input list, of children
--     Int       = index skip factor, in original input list, of children
--     Bool      = True, if decimation in frequency (DIF) was used to form children.
--
-- Notes)
--   1) The radix of the decomposition at any level of a tree is equal
--      to the number of children at that node (i.e. - length subForest),
--      which should also equal the length of the second element in the
--      Tree type tuple.

type GenericLogTree a = Tree (Maybe a, [Int], Int, Bool)

class (t ~ GenericLogTree a) => LogTree t a | t -> a where

    -- evalNode - Evaluates a node in a tree, returning a list of values
    --            of the original type.
    evalNode :: t -> [a]

-- This custom type wrapper around Double allows me to control how they're
-- printed, in order to generate a nicer graph.
newtype PrettyDouble = PrettyDouble {
    value :: Double
  } deriving (Num, Eq, Ord, Fractional, Floating, Real, RealFrac, RealFloat)
instance Show PrettyDouble where
    show = (printf "%10.3g") . zeroThresh . value
        where zeroThresh y =
                if ((abs y) < 1.0e-10)
                then 0.0
                else y

-- FFTTree - an instance of LogTree, this type represents the Fast Fourier
--           Transform (FFT) of arbitrary radix and decimation scheme.
type FFTTree = GenericLogTree (Complex PrettyDouble)
instance LogTree FFTTree (Complex PrettyDouble) where
    evalNode (Node (Just x,  _, _,   _)        _) = [x]
    evalNode (Node (     _,  _, _, dif) children) =
        foldl (zipWith (+)) [0.0 | n <- [1..nodeLen]]
          $ zipWith (zipWith (*)) subTransforms phasors
      where subTransforms =
              if dif then
                [ concat $ transpose -- i.e. - interleave.
                    $ map evalNode
                          [ snd (coProd twiddle child)
                            | twiddle <- twiddles
                          ]
                  | child <- children
                ]
              else map (concat . replicate radix) subs
            subs     = map evalNode children
            childLen = length $ last(levels $ head children)
            nodeLen  = childLen * radix
            radix    = length children
            phasors  = [ [ exp((0.0 :+ (-1.0)) * 2.0 * pi / degree
                             * fromIntegral r * fromIntegral k)
                           | k <- [0..(nodeLen - 1)]]
                         | r <- [0..(radix - 1)]]
            degree   | dif       = fromIntegral radix
                     | otherwise = fromIntegral nodeLen
            twiddles = [ [ exp((0.0 :+ (-1.0)) * 2.0 * pi / fromIntegral nodeLen
                             * fromIntegral m * fromIntegral n)
                           | n <- [0..(childLen - 1)]]
                         | m <- [0..(radix - 1)]]

-- coProd   - Produces a new tree, where the elements are the products
--            of the original elements and the elements of a list.
--            Returns the modified tree and any unconsumed list elements.
coProd :: (Num a, t ~ GenericLogTree a) => [a] -> t -> ([a], t)
coProd [] (Node (Just x,  offsets, skipFactor,   dif) _) =
  ([], Node (Just x,  offsets, skipFactor,   dif) [])
coProd [a] (Node (Just x,  offsets, skipFactor,   dif) _) =
  ([], Node (Just (a * x),  offsets, skipFactor,   dif) [])
coProd (a:as) (Node (Just x,  offsets, skipFactor,   dif) _) =
  (as, Node (Just (a * x),  offsets, skipFactor,   dif) [])
coProd as (Node (_, offsets, skipFactor, dif) children) =
  (bs, Node (Nothing, offsets, skipFactor, dif) childProds)
  where (bs, childProds) = foldl coProdStep (as, []) children

coProdStep :: (Num a, t ~ GenericLogTree a) => ([a], [t]) -> t -> ([a], [t])
coProdStep (as, ts) t = (bs, ts ++ [t'])
    where (bs, t') = coProd as t

-- This is the intended user interface for building trees.
-- It uses the "newtype record syntax" trick of O'Sullivan et al., in
-- order to provide "future proofing" of the implementation by introducing
-- one level of indirection between the user and the actual tree construction.
--
-- This approach has confused many people (including myself). So, I'll attempt
-- to explain what I intend:
--
-- PRIMARY GOAL: We don't want users calling the `TreeBuilder` data constructor
--               directly, because the type of its `buildTree` accessor may
--               change in the future. And that would break exisitng client
--               code.
--
-- SECONDARY GOAL: We don't want to "hard-wire" the types of the input
--                 parameters to the buildTree accessor, because client
--                 code will need to call this function.
--
-- In order to achieve our primary goal we provide, for instance, the
-- `newFFTTree' function, which returns a TreeBuilder instance to the user
-- without requiring him to call TreeBuilder directly. If we then ever
-- change the type of buildTree, we can change the definition of newFFTTree
-- to match, and the user's existing client code won't be affected.
--
-- In order to achieve our secondary goal we encapsulate the input
-- parameters needed by the buildTree function in a new Data item,
-- and use record syntax to set/get the individual fields. In this way,
-- we can expand the data item in the future without breaking existing
-- client code. We add some additional future proofing by forcing the
-- user to use the `newTreeData' convenience function to build his
-- data structure, rather than exporting the TreeData constructor itself.
--
-- So, the call from the user's client code will look like this:
--
--   tData = newTreeData [(2, False), (2, False)] [1.0, 0.0, 0.0, 0.0]
--   tree  = buildTree newFFTTree tData
--
-- The output of the buildTree function has been wrapped inside an
-- Either, in order to make error reporting possible.

data TreeData a = TreeData {
    modes  :: [(Int, Bool)]
  , values :: [a]
} deriving(Show)

{-|
Build a data structure suitable for passing to a tree constructor.

Example:
    tData = newTreeData [(2, False), (2, False)] [1.0, 0.0, 0.0, 0.0]

Note) For now, all booleans in the list should contain
      the same value, either True or False.
-}
newTreeData :: [(Int, Bool)] -- ^ Decomposition modes : (radix, DIF_flag).
            -> [a]           -- ^ Values for populating the tree.
            -> TreeData a    -- ^ Resultant data structure for passing to tree constructor.
newTreeData modes values = TreeData {
                               modes  = modes
                             , values = values
                           }

newtype TreeBuilder t = TreeBuilder {
    -- | Tree builder
    --
    -- Example:
    --   tree  = buildTree newFFTTree tData
    buildTree :: LogTree t a => TreeData a -> Either String t
}

-- | Returns a tree builder suitable for constructing Fast Fourier Transform
--   (FFT) decomposition trees of arbitrary radices and either decimation
--   style (i.e. - DIT or DIF).
newFFTTree :: TreeBuilder FFTTree
--newFFTTree = TreeBuilder ( buildMixedRadixTree . makePrettyData )
newFFTTree = TreeBuilder ( buildMixedRadixTree )
--newFFTTree = TreeBuilder ( fmap (fmap PrettyDouble) . buildMixedRadixTree )
--    where makePrettyData td = TreeData {
--                                  modes  = oldModes
--                                , values = newValues
--                              }
--              where oldModes  = modes td
--                    newValues = map (fmap PrettyDouble) (values td)

-- mixedRadixTree Takes a list of values, along with a list of "radix /
--                decimation-style" preferences, and constructs the tree
--                representing the mixed radix, mixed decimation style
--                decomposition of the list for processing.
--
-- Arguments:
--   modes :: [(Int, Bool)] - list of pairs defining the desired radix and
--                            decimation style for the successive levels of
--                            the decomposition. (The Int gives the radix, and
--                            the Bool tells whether DIF is to be used.)
--
--   xs    :: [a]           - the list of values to be decomposed.

buildMixedRadixTree :: TreeData a -> Either String (GenericLogTree a)
buildMixedRadixTree td = mixedRadixTree td_modes td_values
    where td_modes  = modes td
          td_values = values td

mixedRadixTree :: [(Int, Bool)] -> [a] -> Either String (GenericLogTree a)
mixedRadixTree _     []  = Left "mixedRadixTree(): called with empty list."
mixedRadixTree _     [x] = return $ Node (Just x, [], 0, False) []
mixedRadixTree modes xs  = mixedRadixRecurse 0 1 modes xs

mixedRadixRecurse :: Int -> Int -> [(Int, Bool)] -> [a] -> Either String (GenericLogTree a)
mixedRadixRecurse _ _ _ []  = Left "mixedRadixRecurse(): called with empty list."
mixedRadixRecurse myOffset _ _ [x] = return $ Node (Just x, [myOffset], 0, False) []
mixedRadixRecurse myOffset mySkipFactor modes xs
  | product (map fst modes) == length xs =
    do
      children <- sequence [ mixedRadixRecurse childOffset childSkipFactor
                               (tail modes) subList
                             | (childOffset, subList) <- zip childOffsets subLists
                           ]
      return $ Node (Nothing, childOffsets, childSkipFactor, dif) children
  | otherwise                                    =
      Left "mixedRadixTree(): Product of radices must equal length of input."
  where subLists = [ [xs !! (offset + i * skipFactor) | i <- [0..(childLen - 1)]]
                     | offset <- offsets
                   ]
        childSkipFactor | dif       = mySkipFactor
                        | otherwise = mySkipFactor * radix
        childOffsets    | dif       = [myOffset + (i * mySkipFactor * childLen) | i <- [0..(radix - 1)]]
                        | otherwise = [myOffset +  i * mySkipFactor             | i <- [0..(radix - 1)]]
        skipFactor      | dif       = 1
                        | otherwise = radix
        offsets         | dif       = [i * childLen | i <- [0..(radix - 1)]]
                        | otherwise = [0..(radix - 1)]
        childLen = length xs `div` radix
        radix    = fst $ head modes
        dif      = snd $ head modes

-- This data type enumerates the possible computational operations performed
-- by a computational node.
data CompOp = Sum
            | Prod
-- our computational node type; tuple members are:
-- - list of pairs uniquely identifying the node by its inputs;
--   pair members are:
--   - node ID
--   - field ID
-- - list of multiplicative coefficients to be applied to the inputs
-- - list of computational operations to be used to generate outputs
--
-- All 3 lists should have the same length.
--type CompNode a  = ([(Int, Int)], [a], [CompOp])
type CompNode a  = ([(String, String)], [a], [CompOp])
--type FFTCompNode = CompNode (Complex Double)

-- | Converts a GenericLogTree to a GraphViz dot diagram.
dotLogTree :: (Show a, LogTree t a) => Either String t -> String
dotLogTree (Left msg)   = header
 ++ "\"node0\" [label = \"" ++ msg ++ "\"]\n"
 ++ "}\n"
dotLogTree (Right tree) = header
-- ++ dotLogTreeRecurse "0" tree
 ++ evalState (dotLogTreeRecurse "0" tree) []
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
 \   ranksep = \"1.5\";\n \
 \   nodesep = \"0\";\n"
-- \   edge [ \n \
-- \   ];\n \

dotLogTreeRecurse :: (Show a, LogTree t a) => String -> t -> State [CompNode a] String
dotLogTreeRecurse nodeID (Node (Just x,      offsets,    _,   _)        _) = do -- leaf
    -- Just draw myself.
    return $ "\"node" ++ nodeID ++ "\" [label = \"<f0> "
        ++ "[" ++ show (head offsets) ++ "] " ++ show x
        ++ "\" shape = \"record\"];\n"
dotLogTreeRecurse nodeID (Node (     _, childOffsets, skip, dif) children) = do -- ordinary node
    -- Draw myself.
    let selfStr =
            "\"node" ++ nodeID ++ "\" [label = \"<f0> "
            ++ show (head res)
            ++ (concat [" | <f" ++ show k ++ "> " ++ show val
                         | (val, k) <- zip (tail res) [1..]])
            ++ "\" shape = \"record\"];\n"
    -- Draw children.
    childrenStr <- do
      liftM concat $
        mapM (\(childID, child) ->
          do curState <- get
             let (childStr, newState) =
                   runState (dotLogTreeRecurse childID child) curState
             put newState
             return childStr
          ) [(childID, child) | (childID, child) <- zip childIDs children]
    -- Draw my connections to my children.
    conStrs <-
        forM [0..(num_elems - 1)] (\k -> do
            curState <- get
            let ((compNodeID, compNodeDrawStr), newState) =
                    runState (getCompNodeID (k `mod` num_child_elems) childIDs)
                              curState
            put newState
            return $ compNodeDrawStr ++ (drawConnection nodeID k compNodeID)
                                  )
    -- Return the concatenation of all substrings.
    return (selfStr ++ childrenStr ++ (concat conStrs))
    where num_elems       = length children * num_child_elems
          num_child_elems = length $ last(levels $ head children)
          childIDs        = [nodeID ++ show i | i <- [0..(length children - 1)]]
          res             = evalNode $ Node (Nothing, childOffsets, skip, dif) children
          drawConnection nodeID k compNodeID =
              "\"node"     ++ nodeID  ++ "\":f" ++ show k
           ++ " -> \"node" ++ compNodeID
--            " [id = \"" ++ nodeID ++ childID ++ show k ++ "\"" ++
--            ", decorate = \"true\"" ++
           ++ " [dir = \"back\"];\n"
        --                  ++ ", sametail = \"" ++ nodeID ++ lID ++ (show k) ++ "\""
        --                  ++ ", taillabel = " ++ opt_sign
        --                  ++ ", headlabel = " ++ twiddle
          getCompNodeID :: Int -> [String] -> State [CompNode a] (String, String)
          getCompNodeID k childIDs = do
            compNodes <- get
            let (newCompNodes, compNodeID, compNodeDrawStr) = fetchCompNodeID k childIDs compNodes
            put newCompNodes
            return (compNodeID, compNodeDrawStr)
          fetchCompNodeID :: Int -> [String] -> [CompNode a]
                          -> ([CompNode a], String, String)
          fetchCompNodeID k childIDs compNodes =
            case (findCompNode 0 inputList compNodes) of
              Just foundNodeID -> ( compNodes
                                  , show foundNodeID
                                  , "" -- We don't need to draw anything, if
                                  )    -- the computational node already exists.
              Nothing          -> ( compNodes ++ [(inputList, coeffs, ops)]
                                  , newNodeID
                                  , drawStr
                                  )
                where drawStr   = "\"node1" ++ newNodeID ++ "\""
                               ++ " [shape = \"circle\"]"
                               ++ ";\n"
                               ++ (unlines [ "\"node1" ++ newNodeID ++ "\""
                                          ++ " -> "
                                          ++ "\"node" ++ (fst input) ++ "\""
                                          ++ ":f" ++ (snd input)
                                            | input <- inputList
                                           ])
--                               ++ ";\n"
                      newNodeID = show $ length compNodes
--                      coeffs    = replicate (length inputList) 1::a
                      coeffs    = []
                      ops       = replicate (length inputList) Sum
            where inputList = [ (nodeID, fieldID)
                               | (nodeID, fieldID) <- zip childIDs $
                                                          repeat $ show k
                              ]

          findCompNode :: Int -> [(String, String)] -> [CompNode a] -> Maybe Int
          findCompNode _ _ [] = Nothing
          findCompNode index inputList ((inputs, _, _):cns) =
            if all (== True) [inputItem `elem` inputs | inputItem <- inputList]
            then Just index
            else findCompNode (index + 1) inputList cns

-- Helper function to grab a node's value.
getValue :: LogTree t a => t -> Maybe a
getValue (Node (x, _, _, _) _) = x

-- Helper function to evaluate a node.
getEval (Left msg)   = []
getEval (Right tree) = evalNode tree

-- These helper functions just unwrap the Either from arround a
-- LogTree, so that the equivalent functions from Data.Tree can be used.
getLevels (Left msg)   = [] -- Can't figure out how to usefully cary `msg' forward.
getLevels (Right tree) = levels tree

getFlatten (Left msg)   = [] -- (as above)
getFlatten (Right tree) = levels tree
