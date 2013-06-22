{-# LANGUAGE MultiParamTypeClasses
    , FlexibleInstances
    #-}

module Data.GiST.RTree where

import Data.GiST.GiST
import Data.List(sort)

--data RKey a = RKey (a,a) deriving (Eq,Ord,Show,Read)

-- | R Tree predicate type 
data Predicate a = Contains (a,a) | Equals a deriving (Eq,Ord,Show,Read)

-- | Checks if the intervals of two R Tree predicates overlap
overlaps :: (Ord a) => ((a,a),(a,a)) -> ((a,a),(a,a))  -> Bool
overlaps ((minx1,maxy1),(maxx1,miny1)) ((minx2,maxy2),(maxx2,miny2)) =  (minx1 <= maxx2)
                                                                && (minx2 <= maxx1)
                                                                && (miny1 <= maxy2)
                                                                && (miny2 <= maxy1)

minxP :: Predicate (a,a) -> a
minxP (Contains ((minx,_),(_,_))) = minx
minxP (Equals (x,_)) = x

maxyP :: Predicate (a,a) -> a
maxyP (Contains ((_,maxy),(_,_))) = maxy
maxyP (Equals (_,y)) = y 

maxxP :: Predicate (a,a) -> a
maxxP (Contains ((_,_),(maxx,_))) = maxx
maxxP (Equals (x,_)) = x

minyP :: Predicate (a,a) -> a
minyP (Contains ((_,_),(_,miny))) = miny
minyP (Equals (_,y)) = y

area :: Predicate (Int,Int) -> Int
area (Equals _) = 0
area (Contains ((minx,maxy),(maxx,miny))) = (maxx - minx) * (maxy - miny)

instance Predicates Predicate (Int,Int) where

    consistent (Contains t1) (NodeEntry (_, Contains t2)) = overlaps t1 t2
    consistent (Equals (x,y)) (NodeEntry (_, Contains ((minx,maxy),(maxx,miny)))) = 
                                                            (x >= minx)
                                                            && (x <= maxx)
                                                            && (y >= miny)
                                                            && (y <= maxy)
    consistent (Contains ((minx,maxy),(maxx,miny))) (LeafEntry (_, Equals (x,y))) = 
                                                            (x >= minx)
                                                            && (x <= maxx)
                                                            && (y >= miny)
                                                            && (y <= maxy)       
    consistent (Equals a1) (LeafEntry (_, Equals a2))           = a1 == a2
        
    union ps = Contains ((minx,maxy),(maxx,miny))
                -- | The minimum of all x interval minimums
        where   minx    = minimum $ map minxP ps
                -- | The maximum of all y interval maximums
                maxy    = maximum $ map maxyP ps
                -- | The maximum of all x interval maximums
                maxx    = maximum $ map maxxP ps                 
                -- | The minimum of all y interval minimums
                miny    = minimum $ map minyP ps

    -- | Seperates the sorted list of entries into two halves
    pickSplit es = linearSplit [e1] [e2] [e | e <- es, e /= e1, e/= e2] $ (length es + 1) `div` 2
        where (_, e1, e2) = maximum [greatestPenalty e es | e <- es]

    penalty p1 p2  =  area (union [p1,p2]) - area p2

--unionP :: [Predicate Int] -> Predicate Int
--unionP 
--penaltyP :: Predicate Int -> Predicate Int -> Penalty
--penaltyP p1 p2  =  area (union [p1,p2]) - area p2

greatestPenalty :: Entry Predicate (Int,Int) -> [Entry Predicate (Int,Int)] -> (Penalty, Entry Predicate (Int,Int), Entry Predicate (Int,Int))
greatestPenalty e es = maximum [(penalty (entryPredicate e) (entryPredicate e1), e, e1) | e1 <- es]


linearSplit :: [Entry Predicate (Int, Int)] -> [Entry Predicate (Int,Int)] -> 
    [Entry Predicate (Int,Int)] -> Int -> ([Entry Predicate (Int,Int)], [Entry Predicate (Int,Int)])
linearSplit es1 es2 [] _ = (es1,es2)
linearSplit es1 es2 (e:es) max
    |length es1 == max  = (es1,es2 ++ (e:es))
    |length es2 == max  = (es1 ++ (e:es), es2)
    |otherwise          = if penalty (entryPredicate e) (union $ map entryPredicate es1) >
                            penalty (entryPredicate e) (union $ map entryPredicate es2) 
                            then linearSplit es1 (e:es2) es max
                            else linearSplit (e:es1) es2 es max
