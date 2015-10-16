{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , TypeFamilies
           , Rank2Types
  #-}

{-|
Module      :  Data.Tree.LogTree
Description :  A library for visualizing logarithmic breakdown of computation.
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
module Data.Tree.LogTree (
    LogTree (..), GenericLogTree, TreeData, TreeBuilder (..)
  , CompOp (..), CompNodeOutput, CompNode
  , DecimationType (..), Radix, DecompositionMode
  , newTreeData, buildMixedRadixTree
  , getAllCompNodes
) where

import Data.Complex
import Data.Tree
import Data.List
import Data.Newtypes.PrettyDouble (PrettyDouble(..))

-- | Enumerates the possible computational operations performed by a computational node.
data CompOp = Sum  -- ^ Node sums its inputs.
            | Prod -- ^ Node multiplies its inputs.
    deriving (Eq, Show)

-- | Our computational node type; tuple members are:
--
--       * type of operation (i.e. - CompOp)
--
--       * list of multiplicative coefficients (type a) to be applied to the inputs
type CompNodeOutput a = (CompOp, [a])

-- | Completely defines a particular computational node, by specifying all of its outputs.
--
--   Each tuple in the list corresponds to a unique element of the output list.
type CompNode a       = [CompNodeOutput a]

-- | Enumerates the possible decimation styles of a computation breakdown stage.
data DecimationType = DIT -- ^ Decimation in Time.
                    | DIF -- ^ Decimation in Frequency.
    deriving (Eq, Show)

-- | A convenient type alias to help make the code more readable.
type Radix = Int

-- | A convenient type alias.
--
--   Each stage of a computation breakdown can be uniquely identified by
--   a pair containing:
--
--       [Radix]          Typically, an integer defining the number of
--                        sub-computations being combined in this stage.
--
--       [DecimationType] A flag giving the decimation style used in forming
--                        the list of elements in the sub-computations.
type DecompositionMode = (Radix, DecimationType)

{-|
A convenient type synonym, used as shorthand to specify the actual Tree type.

"a" is the data type of the original input list elements.

Tree type tuple, from left to right:

    * Maybe (a, [[a]]) = original list element value, for leaf; Nothing, otherwise.
                         The list of lists contains the accumulated twiddles.

    * [Int]            = starting indeces, in original input list, of children

    * Int              = index skip factor, in original input list, of children

    * DecimationType   = Flags decimation in time (DIT), or frequency (DIF).

Notes:

    * The radix of the decomposition at any level of a tree is equal
      to the number of children at that node (i.e. - length subForest),
      which should also equal the length of the second element in the
      Tree type tuple.
-}
type GenericLogTree a = Tree (Maybe (a, [[a]]), [Int], Int, DecimationType)

{-|
A class of tree structures representing logarithmic decomposition of arbitrary
radix and using either decimation-in-time (DIT), or decimation-in-frequency
(DIF) approach (or, a mix of both).
-}
class (Show a, t ~ GenericLogTree a) => LogTree t a | t -> a where

    -- | Evaluates a node in a tree, returning a list of values of the original
    --   type. The supplied list of integers gives the index of the outer
    --   summation, for each step in the computational breakdown.
    evalNode       :: (t, [Int]) -> [a]

    -- | Returns any necessary "twiddle" factors, for DIF decomposition.
    getTwiddles    :: t -> [[a]]

    -- | The actual twiddle factor calculator.
    calcTwiddles   :: DecimationType -> Int -> Int -> [[a]]

    -- | Returns the string representations of the twiddle factors.
    getTwiddleStrs :: t -> [[String]]
    getTwiddleStrs = map (map show) . getTwiddles

    -- | Returns the list of computational nodes required,
    --   in order to combine a node's children.
    getCompNodes   :: t -> [CompNode a]

-- | Returns the complete list of computational nodes required,
--   in order to evaluate the tree.
getAllCompNodes :: (t ~ GenericLogTree a, LogTree t a) => t -> [CompNode a]
getAllCompNodes t = getCompNodes t
                     ++ (concatMap getAllCompNodes (subForest t))

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
--   tData = newTreeData [(2, DIT), (2, DIT)] [1.0, 0.0, 0.0, 0.0]
--   tree  = buildTree newFFTTree tData
--
-- The output of the buildTree function has been wrapped inside an
-- Either, in order to make error reporting possible.

-- | Data structure used by tree builders.
--
--   Fields:
--
--       [@modes@]  A list of 'DecompositionMode's
--
--       [@values@] The list of values to be transformed.
data TreeData a = TreeData {
    modes  :: [DecompositionMode]
  , values :: [a]
} deriving(Show)

{-|
Build a data structure suitable for passing to a tree constructor.

Example:
    tData = newTreeData [(2, DIT), (2, DIT)] [1.0, 0.0, 0.0, 0.0]
-}
newTreeData :: [DecompositionMode] -- ^ Decomposition modes : (radix, DIF_flag).
            -> [a]                 -- ^ Values for populating the tree.
            -> TreeData a          -- ^ Resultant data structure for passing to tree constructor.
newTreeData modes values = TreeData {
                               modes  = modes
                             , values = values
                           }

{-|
Example: tree  = buildTree newFFTTree tData

Note:

Please, don't use the `TreeBuilder' data constructor directly, in your client
code, unless you are defining a new instance of typeclass `LogTree'.
You will short circuit our data abstraction strategy, if you do this.
This will expose your client code to potential breakage, in the future.
(See the FFTTree.hs file for an example of how to create a new instance of
typeclass, LogTree.)
-}
newtype TreeBuilder t = TreeBuilder {
    buildTree :: LogTree t a => TreeData a -> Either String t
}

-- Presumably, future contributors will add new tree types, by declaring
-- new instances of `LogTree', along with associated definitions of
-- `evalNode', etc. Because we're using data abstraction
-- to preserve the integrity and longevity of the interface, those new
-- instances will require new, user accessible helper functions, like
-- `newFFTTree', above. The following template is provided, for guidance.
--
-- newFooTree :: TreeBuilder FooTree
-- newFooTree = TreeBuilder buildMixedRadixTree
--
-- Note that the only difference is the type parameter, `t', supplied
-- to `TreeBuilder'. This is because these helper functions are really
-- just conduits of type information, which allow the compiler to:
--  - correctly overload `evalNode', etc., and
--  - correctly type cast the list of user input values, which are
--    often untyped floating point constants.
-- (At least, I think that's what's going on; gurus?)

-- This "helper shim" just isolates the complicated type signature of
-- the recursive tree building function, `mixedRadixRecurse', from the
-- abstracted tree construction machinery, above. It does this in two ways:
--  - It supplies the '0' and '1', which are always the same in the
--    first call to `mixedRadixRecurse', and
--  - it performs the deconstruction of the TreeData structure, so that
--    that deconstruction has to occur neither above, where it would
--    pollute the simplicity of the interface, nor below, where it would
--    be expensive, since `mixedRadixRecurse' is recursive. As paart of
--    this step, it primes the twiddles for each value with an empty list.

{-| NOT FOR USE BY GENERAL CLIENT CODE!

This has been exported, solely for use by definers of new instances
of typeclass, LogTree. (See the FFTTree.hs file, for an example
of how to do this.)
-}
buildMixedRadixTree :: (LogTree t a) => TreeData a -> Either String t
buildMixedRadixTree td = mixedRadixRecurse 0 1 td_modes td_values
    where td_modes  = modes td
          td_values = zip (values td) (repeat [])

-- mixedRadixRecurse Recursively constructs the tree representing the
--                   mixed radix, mixed decimation style decomposition
--                   of the list for processing.
--
-- Arguments:
--
--   myOffset :: Int        - The offset, in the original list of values,
--                            of the first element of `xs'.
--                            (Maintained for graphing purposes.)
--
--   mySkipFactor :: Int    - The distance, in the original list of values,
--                            between consecutive elements of `xs'.
--                            (Maintained for graphing purposes.)
--
--   modes :: [DecompositionMode]
--
--   xs :: [(a, [[a]])]     - the list of values to be decomposed, along with
--                            any twiddles already accumulated in previous
--                            decomposition steps.
--                            (i.e. - the seed of the tree)

mixedRadixRecurse :: (LogTree t a) => Int -> Int -> [DecompositionMode] -> [(a, [[a]])] -> Either String t
mixedRadixRecurse _ _ _ []  = Left "mixedRadixRecurse(): called with empty list."
mixedRadixRecurse myOffset _ _ [(x, ws)] = return $ Node (Just (x, ws), [myOffset], 0, DIT) []
mixedRadixRecurse myOffset mySkipFactor modes subComps
  | product (map fst modes) /= length subComps =
    Left "mixedRadixRecurse: Product of radices must equal length of input."
  | otherwise =
    do
      children <- sequence [ mixedRadixRecurse childOffset childSkipFactor
                               (tail modes) subList
                             | (childOffset, subList) <- zip childOffsets subLists
                           ]
      return $ Node (Nothing, childOffsets, childSkipFactor, decimationType) children
  where subLists =
          [ [ addTwiddle (subComps !! ind) (twiddles' !! (ind `mod` childLen))
              | i <- [0..(childLen - 1)]
              , let ind = offset + i * skipFactor
            ]
            | offset <- offsets
          ]
        addTwiddle (x, ws) w = (x, ws ++ [w])
        childSkipFactor | decimationType == DIF = mySkipFactor
                        | otherwise = mySkipFactor * radix
        childOffsets    | decimationType == DIF = [myOffset + (i * mySkipFactor * childLen) | i <- [0..(radix - 1)]]
                        | otherwise = [myOffset +  i * mySkipFactor             | i <- [0..(radix - 1)]]
        skipFactor      | decimationType == DIF = 1
                        | otherwise = radix
        offsets         | decimationType == DIF = [i * childLen | i <- [0..(radix - 1)]]
                        | otherwise = [0..(radix - 1)]
        childLen = length subComps `div` radix
        radix    = fst $ head modes
        decimationType = snd $ head modes
        twiddles = calcTwiddles decimationType childLen radix
        twiddles' = transpose twiddles
