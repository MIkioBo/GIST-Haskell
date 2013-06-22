{-# LANGUAGE MultiParamTypeClasses
    ,FlexibleInstances
    ,FlexibleContexts
    #-}

module Data.GiST.Types where

data GiST p a  = Leaf [LeafEntry p a] | Node [NodeEntry p a] | Null deriving (Eq, Show, Read)
data Entry p a = LeafEntry (LeafEntry p a) | NodeEntry (NodeEntry p a) deriving (Eq, Show, Read) 

unLeafEntry  (LeafEntry l) =  l
unNodeEntry  (NodeEntry n) =  n

entryPredicate (LeafEntry e) = snd e
entryPredicate (NodeEntry e) = snd e

type LeafEntry p a = (a, p a)

type NodeEntry p a = (GiST p a, p a)

type Penalty = Int

instance  (Eq a, Ord (p a)) => Ord (Entry p a) where
    (<=) (LeafEntry (_,p1)) (LeafEntry (_,p2)) = p1 <= p2
    (<=) (NodeEntry (_,p1)) (NodeEntry (_,p2)) = p1 <= p2
    (<=) (NodeEntry (_,p1)) (LeafEntry (_,p2)) = p1 <= p2
    (<=) (LeafEntry (_,p1)) (NodeEntry (_,p2)) = p1 <= p2

class (Eq a, Eq (p a)) => Predicates p a where
    -- | Checks if the given entry is consistent with a given predicate
    consistent  :: p a -> Entry p a -> Bool
    -- | Returns a predicate that is the union of all predicates of the given list of entries
    union       :: [p a] -> p a
    -- | Calculates a numerical penalty for inserting the entry containing the first predicate 
    -- into a subtree rooted at an entry containing the second predicate
    penalty     :: p a -> p a -> Penalty
    -- | Given a list of entries, returns two disjunct subsets that contain the entries in the list
    -- Focus is on minimising the fill factor
    pickSplit   :: [Entry p a] -> ([Entry p a], [Entry p a])


