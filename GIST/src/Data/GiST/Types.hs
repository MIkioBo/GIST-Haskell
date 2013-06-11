{-# LANGUAGE MultiParamTypeClasses
    ,FlexibleInstances
    ,FlexibleContexts#-}

module Data.GiST.Types where

data GiST p a  = Leaf [LeafEntry p a] | Node [NodeEntry p a] | Null deriving (Eq, Show) -- | OrderedLeaf [OrderedLeafEntry a]
data Entry p a = LeafEntry (LeafEntry p a) | NodeEntry (NodeEntry p a) deriving (Eq, Show) -- | OrderedEntry(OrderedLeafEntry a) 

unLeafEntry  (LeafEntry l) =  l
unNodeEntry  (NodeEntry n) =  n

entryPredicate (LeafEntry e) = snd e
entryPredicate (NodeEntry e) = snd e

type LeafEntry p a = (a, p a)

type NodeEntry p a = (GiST p a, p a)

type Penalty = Int
type Level = Int

instance  (Eq a, Ord (p a)) => Ord (Entry p a) where
    (>) (LeafEntry (_,p1)) (LeafEntry (_,p2)) = p1 > p2
    (>) (NodeEntry (_,p1)) (NodeEntry (_,p2)) = p1 > p2

class (Eq a, Eq (p a)) => Predicates p a where
    -- | Checks if the given entry is consistent with a given predicate
    consistent  :: p a -> Entry p a -> Bool
    -- | Returns a predicate that is the union of all predicates of the given list of entries
    union       :: [Entry p a] -> p a
    -- | Calculates a numerical penalty for inserting the first entry into a subtree rooted in the second
    penalty     :: LeafEntry p a -> NodeEntry p a -> Penalty
    -- | Given a list of entries, returns two disjunct subsets that contain the entries in the list
    -- Focus is on minimising the fill factor
    pickSplit   :: [Entry p a] -> ([Entry p a], [Entry p a])

class (Predicates p a) => GiSTs g p a where
    -- | Searches the GiST for leaf nodes that satisfy the given search predicate
    search  :: g p a -> p a -> [LeafEntry p a]
    -- | Inserts an entry into the tree, rebalancing the tree if necessary
    insert  :: g p a -> (Int, Int) -> LeafEntry p a -> Level -> g p a
       -- | Deletes a leaf entry from the tree and rebalances if necessary 
    delete  :: g p a -> (Int, Int) -> LeafEntry p a -> g p a 
    
    empty   :: g p a 
