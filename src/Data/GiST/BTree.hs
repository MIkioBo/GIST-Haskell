{- |
Module      :  BTree
Copyright   :  (c) Mihail Bogojeski
License     :  GPL

Maintainer  :  mihailbogojeski@gmail.com
Stability   :  experimental
Portability :  portable

A simple implementation of the B+ tree predicate. A containment predicate is a tuple of two integers
representing an open interval, while the equality predicate is simply an interger value.
-}

{-# LANGUAGE MultiParamTypeClasses
    #-}



module Data.GiST.BTree (
    Predicate (..)
    ,between
) where

import Data.GiST.GiST(Entry(..),entryPredicate,Predicates(..),Penalty)
import Data.List(sort)



data Predicate a = Contains (a,a)                   -- ^ containment predicate (interval)
                 | Equals a                         -- ^ equality predicate (integer value)
                 deriving (Eq, Ord, Show, Read)



-- | More documentation on the instance implementation in the source
instance Predicates Predicate Int where
    
    -- | Two containment predicates are consistent if the intervals they represent overlap
    -- A containment and equality predicate are consistent if the interval represented by the former contains the value of the latter
    -- Two equality predicates are consistent if they represent the same value
    consistent (Contains (min1,max1)) (NodeEntry (_, Contains (min2,max2)))  = (min1 <= max2) && (max1 >= min2)
    consistent (Equals a) (NodeEntry (_, Contains (min,max)))   = between a min max 
    consistent (Contains (min,max)) (LeafEntry (_, Equals a))   = between a min max
    consistent (Equals a1) (LeafEntry (_, Equals a2))           = a1 == a2
    
    -- | A union of predicates is an interval spanning from the minimal 
    -- to the maximal value of all the predicats
    union ps = Contains (min,max)
                -- The minimum of all interval minimums
        where   min         = minimum $ map minP ps
                -- The maximum of all interval maximums
                max         = maximum $ map maxP ps
    
    -- | Seperates the sorted list of entries into two halves
    pickSplit es = splitAt ((length es + 1) `div` 2) sorted
        where   sorted  = sort es
    -- | The distance between the intervals (or values) of the predicates
    penalty p1 p2 = maximum[(minP p2)-(minP p1), 0] + maximum [(maxP p1)-(maxP p2),0]

-- The lower limit of the predicate
minP :: Predicate a -> a
minP (Contains (min,_)) = min
minP (Equals a) = a

-- The upper limit of the predicate
maxP :: Predicate a -> a
maxP (Contains (_,max)) = max
maxP (Equals a) = a

-- | Tests if a value is between two others
between :: Ord a => a -> a -> a -> Bool
between a min max = (a >= min) && (a <= max)
