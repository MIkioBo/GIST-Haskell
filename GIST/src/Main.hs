{-# LANGUAGE MultiParamTypeClasses
    ,FlexibleInstances#-}
module Main where

import Data.GiST
import Data.List

main = do (putStrLn.show) "lol"

newtype BTreePredicate a = BTP (a,a) deriving (Eq, Ord, Show)

unBTP (BTP a) = a

-- | Checks if the intervals of two BTreePredicates overlap
overlaps :: (Ord a) => BTreePredicate a -> BTreePredicate a -> Bool
overlaps (BTP (min1, max1)) (BTP (min2, max2)) = (min1>=min2 && min1<=max2) || (max1>=min2 && max1<=max2)  


instance Predicates BTreePredicate Int where
    
    consistent p1 (NodeEntry (_, p2)) = overlaps p1 p2 
    consistent p1 (LeafEntry (_, p2)) = overlaps p1 p2
    
    union es = BTP (min,max)
        where   predicates  = map unBTP $ map entryPredicate es
                -- | The minimum of all interval minimums
                min         = minimum $ map fst predicates
                -- | The maximum of all interval maximums
                max         = maximum $ map snd predicates
    
    -- | Seperates the sorted list of entries into two halves
    pickSplit es =  (take ((length es) `div` 2) $ sort es, drop ((length es) `div` 2) $ sort es) 

    penalty (_, BTP (min1,max1)) (_, BTP(min2,max2)) = maximum[min2-min1, 0] + maximum [max1-max2,0]


