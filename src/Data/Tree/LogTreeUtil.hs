{-|
Module      :  Data.Tree.LogTreeUtil
Description :  A package of utilities for working with 'LogTree' instances.
Copyright   :  Copyright (c) 2014 David Banas; all rights reserved World wide.
License     :  BSD3

Maintainer  :  capn.freako@gmail.com
Stability   :  Development
Portability :
-}
module Data.Tree.LogTreeUtil (
    dotLogTree, getLevels, getFlatten, getEval
) where

import Control.Monad.State.Lazy
import Text.Printf (printf, PrintfArg)
import Data.Tree
import Data.List
import Data.Tree.LogTree

-- | Converts a GenericLogTree to a GraphViz dot diagram.
dotLogTree :: (Show a, Eq a, Num a, LogTree t a) => Either String t -> (String, String)
dotLogTree (Left msg)   = (header
 ++ "\"node0\" [label = \"" ++ msg ++ "\"]\n"
 ++ "}\n", "")
dotLogTree (Right tree) = (header
 ++ treeStr
 ++ "}\n",
 compNodeLegend)
    where (treeStr, compNodeTypes) = runState (dotLogTreeRecurse "0" (getCompNodes tree) tree twiddles) []
          -- This is just a convenient way to get a list of correctly typed "1.0"s of the correct length:
          twiddles       = concat $ getTwiddleStrs $ Node (Nothing, [], 0, DIT) $ subForest tree
          nodeLen        = fromIntegral $ length $ last (levels tree)
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
dotLogTreeRecurse :: (Show a, Eq a, Num a, LogTree t a) => String -> [CompNode a] -> t -> [String] -> State [CompNode a] String
dotLogTreeRecurse nodeID         _ (Node (Just x,      offsets,    _,   _)        _) twiddleVec =    -- leaf
    -- Just draw myself.
    return $ "\"node" ++ nodeID ++ "\" [label = \"<f0> "
        ++ "[" ++ show (head offsets) ++ "] " ++ show (fst x)
        ++ "\" shape = \"record\"];\n"
dotLogTreeRecurse nodeID compNodes (Node (     _, childOffsets, skip, dif) children) twiddleVec = do -- ordinary node
    -- Draw myself.
    let selfStr =
            "\"node" ++ nodeID ++ "\" [label = \"<f0> "
            ++ show (head res) ++ head twiddleVec
            ++ concat [" | <f" ++ show k ++ "> " ++ show val ++ twiddle
                        | ((val, k), twiddle) <- zip (zip (tail res) [1..]) (tail twiddleVec)]
            ++ "\" shape = \"record\"];\n"
    -- Draw children.
    childrenStr <- liftM concat $
        mapM (\((childID, child), twiddleVec) ->
              do curState <- get
                 let (childStr, newState) =
                       runState (dotLogTreeRecurse childID (getCompNodes child) child twiddleVec) curState
                 put newState
                 return childStr
             ) $ zip (zip childIDs children) twiddles
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
          res             = evalNode (Node (Nothing, childOffsets, skip, dif) children, [])
          twiddles        = getTwiddleStrs $ Node (Nothing, [], 0, dif) children
          twiddleVals     = getTwiddles $ Node (Nothing, [], 0, dif) children
          twiddleChoice   = last twiddleVals
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
getValue (Node (Just (x, _), _, _, _) _) = Just x
getValue (Node (Nothing, _, _, _) _) = Nothing

-- | Helper function to evaluate a node.
getEval (Left msg)   = []
getEval (Right tree) = evalNode (tree, [])

-- | These helper functions just unwrap the 'Either' from arround a
--   'LogTree', so that the equivalent functions from 'Data.Tree' can be used.
getLevels (Left msg)   = [] -- Can't figure out how to usefully cary `msg' forward.
getLevels (Right tree) = levels tree

-- | These helper functions just unwrap the 'Either' from arround a
--   'LogTree', so that the equivalent functions from 'Data.Tree' can be used.
getFlatten (Left msg)   = [] -- (as above)
getFlatten (Right tree) = flatten tree
