{-# LANGUAGE MultiParamTypeClasses
    ,FlexibleInstances
    ,FlexibleContexts
    #-}

module Data.GiST.GiST (module Data.GiST.Types, GiSTs(..), getEntries) where

import Data.GiST.Types

class (Predicates p a) => GiSTs g p a where
    -- | Searches the GiST for leaf nodes that satisfy the given search predicate
    search  :: p a -> g p a -> [a]
    -- | Inserts an entry into the tree, rebalancing the tree if necessary
    insert  :: LeafEntry p a -> (Int, Int) -> g p a -> g p a
    -- | Deletes a leaf entry from the tree and rebalances if necessary 
    delete  :: LeafEntry p a -> (Int, Int) -> g p a -> g p a
    -- | Create a new empty GiST
    empty   :: g p a

instance (Predicates p a) => GiSTs GiST p a where

    search  p (Leaf es)     = [fst e | e <- es, consistent p (LeafEntry e)] 
    search  _ (Node [])     = []
    search  p (Node (e:es))
        |consistent p (NodeEntry e) = (search p (fst e)) ++ (search p (Node es))
        |otherwise                  = search p (Node es)
  
    
    insert (toIns, pred) (min,max) (Node es)
            |search pred (Node es) /= [] = Node es
            |length newEs2 <= max   =  Node newEs2
            |otherwise              = Node [(Node $ map unNodeEntry es1, union $ map entryPredicate es1)
                                        ,(Node $ map unNodeEntry es2, union $ map entryPredicate es2)] 
                -- | The new entries after inserting
        where   newEs =  [if (e == minSubtree)
                            then oldSub
                            else e
                        |e <- es]
                -- | The optimal subtree to insert into
                minSubtree = chooseSubtree es (toIns,pred)
                -- | The changed (and additional) subtree after insert
                (oldSub,newSub) = insertGiST minSubtree (min,max) (toIns,pred)
                -- | New entries after possible addition of splitted subtree
                newEs2 = if ((fst newSub) == Null) then newEs else newSub:newEs
                -- | The split of the node entries (in case of overpopulation)
                (es1,es2) =  pickSplit $ map NodeEntry newEs2 
             
    insert (toIns, p) (min,max) (Leaf es)
            |search p (Leaf es) /= [] = Leaf es
            |length newEs <= max    = Leaf newEs
            |otherwise              = Node [(Leaf $ map unLeafEntry es1,union $ map entryPredicate es1)
                                        ,(Leaf $ map unLeafEntry es2, union $ map entryPredicate es2)] 
                -- | The new entries after insert
        where   newEs = (toIns, p):es
                -- | The split of the node entries (in case of overpopulation)
                (es1,es2) =  pickSplit $ map LeafEntry newEs 
    
    
    delete (toDel, p) (min,max) (Node es) 
            |length newEs == 1  = insertMultiple toAdd (makeRoot $ head newEs) (min,max) 
            |otherwise          = insertMultiple toAdd (Node newEs) (min, max)
                -- | The new entries after delete without Null entries
        where   newEs = filter (not.isNull) (map fst delNodes)  
                -- | The propagated entries to add
                toAdd = concat (map snd delNodes)
                -- | The entries after delete
                delNodes =  [if (consistent p (NodeEntry e)) 
                                then (deleteAndCondense e (min,max) (toDel,p))
                                else (e,[])
                            |e <- es]
                            
    delete (toDel, p) (min,max) (Leaf es) = Leaf [e | e <- es, not $ consistent p (LeafEntry e)]
        
    
    empty = Leaf []
   

-- | A helper function that propagates deletion through the subtrees
deleteAndCondense :: (Predicates p a) => NodeEntry p a -> (Int,Int) -> LeafEntry p a -> (NodeEntry p a, [LeafEntry p a])
deleteAndCondense (Node es, pred) (min, max) (toDel, p)
        |length newEs < min = ((Null, pred), toAdd ++ getEntries (Node es))   
        |otherwise          = ((Node newEs, union $ map snd newEs), toAdd)
            -- | The new entries after delete without Null entries
    where   newEs = filter (not.isNull) (map fst delNodes)  
            -- | The propagated entries to add
            toAdd = concat (map snd delNodes) 
            -- | The entries after delete
            delNodes =  [if (consistent p (NodeEntry e)) 
                            then (deleteAndCondense e (min,max) (toDel,p))
                            else (e,[])
                        |e <- es]

deleteAndCondense ((Leaf es),pred) (min, max) (toDel, p) 
    |length newEs < min = ((Null,pred), newEs)
    |otherwise          = ((Leaf newEs, union $ map snd newEs),[])        
                -- | The new entries after delete without Null entries
        where   newEs = [e | e <- es, not $ consistent p (LeafEntry e)] 



-- | A helper function that propagates insertion through the subtrees        
insertGiST :: (Predicates p a) => NodeEntry p a -> (Int,Int) -> LeafEntry p a -> (NodeEntry p a, NodeEntry p a)
insertGiST (Node es,p) (min,max) (toIns,pred)
            |length newEs2 <= max  =  ((Node newEs2,union $ map snd newEs2),(Null, p) )
            |otherwise =((Node  (map unNodeEntry es1), union $ map entryPredicate es1)
                        ,(Node (map unNodeEntry es2), union $ map entryPredicate es2)) 
                -- | The new entries after insert
        where   newEs =   [if (e == minSubtree)
                            then oldSub 
                            else e
                          |e <- es]
                
                -- | The optimal subtree to insert into
                minSubtree = chooseSubtree es (toIns,pred)
                -- | The changed (and additional) subtree after insert
                (oldSub,newSub) = insertGiST minSubtree (min,max) (toIns,pred)
                -- | New entries after possible addition of splitted subtree
                newEs2 = if ((fst newSub) == Null) then newEs else newSub:newEs
                -- | The split of the node entries (in case of overpopulation)
                (es1,es2) =  pickSplit $ map NodeEntry newEs2 

insertGiST (Leaf es,p) (min,max) (toIns,pred)
            |length newEs <= max  =  ((Leaf newEs,union $ map snd newEs),(Null, p) )
            |otherwise =((Leaf (map unLeafEntry es1), union $ map entryPredicate es1)
                        ,(Leaf (map unLeafEntry es2), union $ map entryPredicate es2)) 
            -- | The optimal subtree to insert into
    where   newEs = ((toIns,pred) : es)
            -- | The split of the node entries (in case of overpopulation)
            (es1,es2) =  pickSplit $ map LeafEntry newEs 



-- | Inserts multiple entries into a GiST   
insertMultiple :: (Predicates p a) => [LeafEntry p a] -> GiST p a -> (Int,Int) -> GiST p a
insertMultiple [] gist _ = gist
insertMultiple (e:es) gist (min,max) = insertMultiple es afterInsert (min,max)
    where afterInsert = insert e (min,max) gist



-- | Chooses the most appropriate subtree to insert the entry into
chooseSubtree   :: (Predicates p a )=>[(NodeEntry p a)] -> LeafEntry p a -> (NodeEntry p a) 
chooseSubtree subtrees e    = minPenalty penalties (head penalties)
        where   penalties = [(ne, penalty (snd e) (snd ne))|ne <- subtrees]


-- | Return the minimum penalty and corresponding entry from a list od entries and penalties
minPenalty :: [(NodeEntry p a, Penalty)]->(NodeEntry p a, Penalty) -> NodeEntry p a
minPenalty [] p = fst p 
minPenalty ((ne, pen):ps) (nemin, minpen) 
    |pen < minpen   = minPenalty ps (ne, pen) 
    |otherwise      = minPenalty ps (nemin, minpen) 


-- | If a node has less than the minimum number of entries, borrows entries from another node or
-- | deletes the node and reinserts all its entries
condenseTree    ::(Eq a, Predicates p a) => GiST p a -> (Int, Int) -> GiST p a
condenseTree gist (min, max) = gist


-- | Takes an entry and extracts the GiST from it
makeRoot :: NodeEntry p a -> GiST p a
makeRoot (Node es, p) = Node es
makeRoot (Leaf es, p) = Leaf es


-- | Checks if an entry contains a Null GiST
isNull :: NodeEntry p a -> Bool
isNull (Null, p) = True
isNull _ = False


-- | Returns all leaf entries stored in a GiST
getEntries :: GiST p a -> [LeafEntry p a]
getEntries (Node  es) = concat [getEntries sub | (sub,p) <- es] 
getEntries (Leaf  es) = es
