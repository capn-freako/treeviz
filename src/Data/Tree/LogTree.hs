{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , TypeFamilies
           , Rank2Types
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
) where

import Data.Complex
import Data.Tree
import Data.List
import Text.Printf (printf, PrintfArg)
import Control.Monad.State.Lazy
import Data.Newtypes.PrettyDouble (PrettyDouble(..))

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

data CompOp = Sum -- Enumerates the possible computational operations performed by a computational node.
            | Prod
    deriving (Eq)
-- Our computational node type; tuple members are:
-- - type of operation (i.e. - CompOp)
-- - list of multiplicative coefficients (type a) to be applied to the inputs
--
-- Each tuple in the list corresponds to a unique element of the output list.
type CompNodeOutput a = (CompOp, [a])
type CompNode a        = [CompNodeOutput a]

type GenericLogTree a = Tree (Maybe a, [Int], Int, Bool)

class (t ~ GenericLogTree a) => LogTree t a | t -> a where
    evalNode     :: t -> [a]          -- Evaluates a node in a tree, returning a list of values of the original type.
    getCompNodes :: t -> [CompNode a] -- Returns the complete list of computational nodes required, in order to
                                      -- evaluate the tree.

-- FFTTree - an instance of LogTree, this type represents the Fast Fourier
--           Transform (FFT) of arbitrary radix and decimation scheme.
type FFTTree = GenericLogTree (Complex PrettyDouble)
instance LogTree FFTTree (Complex PrettyDouble) where
    evalNode (Node (Just x,  _, _,   _)        _) = [x] -- The evaluation of a leaf is itself.
    evalNode (Node (     _,  _, _, dif) children) =     -- Sub-trees are evaluated recursively, but require inclusion
        foldl (zipWith (+)) [0.0 | n <- [1..nodeLen]]   -- of `phasors' & potential `twiddle factors'.
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

    getCompNodes (Node ( Just x, _, _,   _)        _) = [] -- A leaf requires no computational nodes.
    getCompNodes (Node (Nothing, _, _, dif) children) =
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
                degree   | dif       = fromIntegral radix
                         | otherwise = fromIntegral nodeLen

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
newFFTTree = TreeBuilder buildMixedRadixTree

buildMixedRadixTree :: TreeData a -> Either String (GenericLogTree a)
buildMixedRadixTree td = mixedRadixTree td_modes td_values
    where td_modes  = modes td
          td_values = values td

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

-- | Converts a GenericLogTree to a GraphViz dot diagram.
dotLogTree :: (Show a, Eq a, LogTree t a) => Either String t -> (String, String)
dotLogTree (Left msg)   = (header
 ++ "\"node0\" [label = \"" ++ msg ++ "\"]\n"
 ++ "}\n", "")
dotLogTree (Right tree) = (header
 ++ treeStr
 ++ "}\n",
 compNodeLegend)
    where (treeStr, compNodeTypes) = runState (dotLogTreeRecurse "0" (getCompNodes tree) tree) []
          compNodeLegend = "digraph {\n"
            ++ "label = \"Computational Node Legend\" fontsize = \"24\"\n"
            ++ "\"node0L\""
            ++ " [label = <<table border=\"0\" cellborder=\"0\" cellpadding=\"3\" bgcolor=\"white\"> \\ \n"
            ++ unlines indexedStrs
            ++ "</table>>, shape = \"Mrecord\""
            ++ "];\n}\n"
          indexedStrs = map (\str -> "<tr> \\ \n" ++ str ++ "</tr> \\") legendStrs
          legendStrs  = map (\(nodeType, typeInd) ->
            concat $ ("  <td align=\"right\">" ++ show typeInd ++ ":</td> \\ \n") : outSpecs nodeType
                                  ) $ zip compNodeTypes [0..]
          outSpecs :: (Show a) => CompNode a -> [String]
          outSpecs nodeOutputs = map (\(nodeOutput, yInd) ->
            let opStr = case fst nodeOutput of
                                        Sum  -> " + "
                                        Prod -> " * "
            in "    <td align=\"left\">y" ++ show yInd ++ " = "
                 ++ intercalate opStr (map (\(coeff, k) -> "(" ++ show coeff ++ printf ") * x%d" k)
                                             $ zip (snd nodeOutput) [(0::Int)..])
                 ++ "</td> \\ \n"
                                     ) $ zip nodeOutputs [(0::Int)..]

header = "digraph g { \n \
 \   ranksep = \"1.5\";\n \
 \   nodesep = \"0\";\n \
 \   label = \"Divide & Conquer Processing Graph\";\n \
 \   labelloc = \"t\";\n \
 \   fontsize = \"28\" \n \
 \   graph [ \n \
 \       rankdir = \"RL\" \n \
 \       splines = \"false\" \n \
 \   ]; \n \
 \   node [ \n \
 \       fontsize = \"16\" \n \
 \       shape = \"circle\" \n \
 \       height = \"0.3\" \n \
 \   ]; \n \
 \   edge [ \n \
 \       dir = \"back\" \n \
 \   ];\n"

-- The two [CompNode a]s here are confusing. The one that comes in as
-- the second argument to the function is the actual list of computational
-- nodes in the diagram. The one that is the accumulated state of the State
-- monad is a list of the different TYPES of computational nodes required,
-- in order to evaluate the tree.
dotLogTreeRecurse :: (Show a, Eq a, LogTree t a) => String -> [CompNode a] -> t -> State [CompNode a] String
dotLogTreeRecurse nodeID         _ (Node (Just x,      offsets,    _,   _)        _) =    -- leaf
    -- Just draw myself.
    return $ "\"node" ++ nodeID ++ "\" [label = \"<f0> "
        ++ "[" ++ show (head offsets) ++ "] " ++ show x
        ++ "\" shape = \"record\"];\n"
dotLogTreeRecurse nodeID compNodes (Node (     _, childOffsets, skip, dif) children) = do -- ordinary node
    -- Draw myself.
    let selfStr =
            "\"node" ++ nodeID ++ "\" [label = \"<f0> "
            ++ show (head res)
            ++ concat [" | <f" ++ show k ++ "> " ++ show val
                        | (val, k) <- zip (tail res) [1..]]
            ++ "\" shape = \"record\"];\n"
    -- Draw children.
    childrenStr <- liftM concat $
        mapM (\(childID, child) ->
          do curState <- get
             let (childStr, newState) =
                   runState (dotLogTreeRecurse childID (getCompNodes child) child) curState
             put newState
             return childStr
          ) [(childID, child) | (childID, child) <- zip childIDs children]
    -- Draw computation nodes between me and my children.
    compNodeStrs <- forM (zip compNodes [0..]) (\(compNode, k') -> do
            let compNodeID = nodeID ++ "C" ++ show k'
            curState <- get
            let (compNodeType, newState) = runState (getCompNodeType compNode) curState
            put newState
            return $ "\"node" ++ compNodeID ++ "\""
               ++ " [label = \"" ++ show compNodeType ++ "\""
               ++ ", shape = \"circle\""
               ++ ", height = \"0.1\"" -- Just making sure it's as small as possible.
               ++ "];\n")
    -- Draw the connections.
    let conexStrs = [
            "\"node" ++ nodeID  ++ "\":f" ++ show (r * childLen + k')
         ++ " -> \"node" ++ nodeID ++ "C" ++ show k' ++ "\""
         ++ " [headlabel = \"y" ++ show r ++ "\" labelangle = \"-30\" labeldistance = \"2\"];\n"
         ++ "\"node" ++ nodeID ++ "C" ++ show k' ++ "\""
         ++ " -> \"node" ++ nodeID ++ show r ++ "\":f" ++ show k'
         ++ " [taillabel = \"x" ++ show r ++ "\" labelangle = \"20\" labeldistance = \"2.5\"];\n"
            | k' <- [0..(length compNodes - 1)]
            , r  <- [0..(length children - 1)]
                    ]
    -- Return the concatenation of all substrings.
    return (selfStr ++ childrenStr ++ concat compNodeStrs ++ concat conexStrs)
    where childIDs        = [nodeID ++ show i | i <- [0..(length children - 1)]]
          childLen        = fromIntegral $ length $ last(levels $ head children)
          res             = evalNode $ Node (Nothing, childOffsets, skip, dif) children
          getCompNodeType :: Eq a => CompNode a -> State [CompNode a] Int
          getCompNodeType compNode = do
            compNodes <- get
            let (newCompNodes, compNodeType) = fetchCompNodeType compNode compNodes
            put newCompNodes
            return compNodeType
          fetchCompNodeType :: Eq a => CompNode a -> [CompNode a] -> ([CompNode a], Int)
          fetchCompNodeType compNode compNodes =
            case findCompNode 0 compNode compNodes of
              Just compNodeIndex -> (compNodes, compNodeIndex)
              Nothing            -> (compNodes ++ [compNode], length compNodes)
          findCompNode :: Eq a => Int -> CompNode a -> [CompNode a] -> Maybe Int
          findCompNode     _        _         [] = Nothing
          findCompNode index compNode (cn : cns) =
            if  compNode == cn
            then Just index
            else findCompNode (index + 1) compNode cns

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
