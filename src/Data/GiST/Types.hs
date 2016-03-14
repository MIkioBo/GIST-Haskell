{- |
Module      :  Types
Copyright   :  (c) Mihail Bogojeski, Alexander Svozil
License     :  GPL

Maintainer  :  mihailbogojeski@gmail.com
Stability   :  experimental
Portability :  portable

Defines the basic types used for this GiST implementation and a couple of operations on these types.
Also introduces the Predicates class which the user of this package can instance to create predicates
which define the data stored in the tree and the tree's behavior during insert, delete, and search operations
-}


{-# LANGUAGE MultiParamTypeClasses
    ,FlexibleInstances
    ,FlexibleContexts
    #-}

module Data.GiST.Types where

import Data.Foldable as F

-- | The data structure used for building the GiST
data GiST p a  = Leaf [LeafEntry p a]       -- ^ leaf node
               | Node [NodeEntry p a]       -- ^ internal node
               | Null                       -- ^ a null GiST
               deriving (Eq, Show, Read)


-- | A general entry type for the gist
data Entry p a = LeafEntry (LeafEntry p a)
               | NodeEntry (NodeEntry p a) 
               deriving (Eq, Show, Read) 

unLeafEntry  (LeafEntry l) =  l
unNodeEntry  (NodeEntry n) =  n

-- | Returns the predicate of this entry
entryPredicate :: Entry p a -> p a
entryPredicate (LeafEntry e) = snd e
entryPredicate (NodeEntry e) = snd e

-- | A leaf entry has a predicate and data
type LeafEntry p a = (a, p a)

-- | A node entry has a predicate and a subtree
type NodeEntry p a = (GiST p a, p a)

type Penalty = Int

-- | Comparison only based on the predicate
instance  (Eq a, Ord (p a)) => Ord (Entry p a) where
    (<=) (LeafEntry (_,p1)) (LeafEntry (_,p2)) = p1 <= p2
    (<=) (NodeEntry (_,p1)) (NodeEntry (_,p2)) = p1 <= p2
    (<=) (NodeEntry (_,p1)) (LeafEntry (_,p2)) = p1 <= p2
    (<=) (LeafEntry (_,p1)) (NodeEntry (_,p2)) = p1 <= p2


-- | The predicate class that can be instanced by the user to create new types
-- of balanced search trees
class (Eq a, Eq (p a)) => Predicates p a where
    -- | Checks if the given entry is consistent with a given predicate
    consistent  :: p a -> Entry p a -> Bool
    -- | Returns a predicate that is the union of all predicates of the given list of entries
    union       :: [p a] -> p a
    -- | Calculates a numerical penalty for inserting the entry containing the first predicate 
    -- into a subtree rooted at an entry containing the second predicate
    penalty     :: p a -> p a -> Penalty
    -- | Given a list of entries, returns two disjunct subsets that contain the entries in the list.
    -- Focus is on minimising the overlap between the splitted entries' predicates
    pickSplit   :: [Entry p a] -> ([Entry p a], [Entry p a])


