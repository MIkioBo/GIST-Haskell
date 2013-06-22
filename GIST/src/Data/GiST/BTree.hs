{-# LANGUAGE MultiParamTypeClasses
    #-}

module Data.GiST.BTree where

import Data.GiST.GiST
import Data.List(sort)

-- | B+ Tree predicate type
data Predicate a = Contains (a,a) | Equals a deriving (Eq, Ord, Show, Read)



minP :: Predicate a -> a
minP (Contains (min,_)) = min
minP (Equals a) = a

maxP :: Predicate a -> a
maxP (Contains (_,max)) = max
maxP (Equals a) = a

-- | B+ Tree predicate instance implementation
instance Predicates Predicate Int where
    
    consistent (Contains (min1,max1)) (NodeEntry (_, Contains (min2,max2)))  = (min1 < max2) && (max1 > min2)
    consistent (Equals a) (NodeEntry (_, Contains (min,max)))   = (a >= min) && (a < max) 
    consistent (Contains (min,max)) (LeafEntry (_, Equals a))   = (a >= min) && (a < max)
    consistent (Equals a1) (LeafEntry (_, Equals a2))           = a1 == a2
        
    union ps = Contains (min,max)
                -- | The minimum of all interval minimums
        where   min         = minimum $ map minP ps
                -- | The maximum of all interval maximums
                max         = maximum $ map maxP ps
    
    -- | Seperates the sorted list of entries into two halves
    pickSplit es =  (take ((length es) `div` 2) $ sort es, drop ((length es) `div` 2) $ sort es) 

    penalty p1 p2 = maximum[(minP p2)-(minP p1), 0] + maximum [(maxP p1)-(maxP p2),0]


