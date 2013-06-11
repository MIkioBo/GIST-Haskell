{-# LANGUAGE MultiParamTypeClasses
    ,FlexibleInstances#-}

module Data.GiST (module Data.GiST.Types) where

import Data.GiST.Types

   


instance (Predicates p a) => GiSTs GiST p a where

    search (Leaf es) p          = [e | e <- es, consistent p (LeafEntry e)] 
    search (Node  []) _            = []
    search (Node  (e:es)) p
        |consistent p (NodeEntry e) = (search (fst e) p) ++ (search (Node es) p)
        |otherwise                  = search (Node es) p
  
    insert (Node es) (min,max) (toIns, pred) 0 
            |length newEs2 <= max  =  Node newEs2
            |otherwise = Node [(Node $ map unNodeEntry es1, union es1),(Node $ map unNodeEntry es2, union es2)] 
        where   newEs =  [if (e == minSubtree)
                            then oldSub
                            else e
                        |e <- es]
                minSubtree = chooseSubtree es (toIns,pred) 0
                (oldSub,newSub) = insertGiST minSubtree (min,max) (toIns,pred) 0
                newEs2 = if ((fst newSub) == Null) then newEs else newSub:newEs
                (es1,es2) =  pickSplit $ map NodeEntry newEs 
             

    insert(Leaf es) (min,max) (toIns, p) 0
            |length newEs <= max    = Leaf newEs
            |otherwise              = Node [(Leaf $ map unLeafEntry es1,union es1), (Leaf $ map unLeafEntry es2, union es2)] 
        where   newEs = (toIns, p):es
                (es1,es2) =  pickSplit $ map LeafEntry newEs 
    
    delete (Node es) (min,max) (toDel, p)
            |length newEs == 1  = insertMultiple (makeRoot $ head newEs) (min,max) toAdd 0
            |otherwise          = insertMultiple (Node newEs) (min, max) toAdd 0
        where   newEs = filter (not.isNull) (map fst delNodes)  
                toAdd = concat (map snd delNodes)
                delNodes =  [if (consistent p (NodeEntry e)) 
                                then (deleteAndCondense e (min,max) (toDel,p))
                                else (e,[])
                            |e <- es]
    delete (Leaf es) (min,max) (toDel,p) = Leaf [e | e <- es, not $ consistent p (LeafEntry e)]
        
    empty = Node []
   
{--instance (Eq a) => Predicates Predicate a where
    consistent e p = True
    union ((LeafEntry (a1,p)):es) = p
    union ((NodeEntry (g, p)):es) = p
    penalty e1 e2 =  0
    pickSplit  (e:es) = [es]
--}

deleteAndCondense :: (Predicates p a) => NodeEntry p a -> (Int,Int) -> LeafEntry p a -> (NodeEntry p a, [LeafEntry p a])
deleteAndCondense (Node es, pred) (min, max) (toDel, p)
        |length newEs < min = ((Null, pred), toAdd ++ getEntries (Node es))   
        |otherwise          = ((Node newEs, union $ map NodeEntry newEs), toAdd)
    where   newEs = filter (not.isNull) (map fst delNodes)  
            toAdd = concat (map snd delNodes)
            delNodes =  [if (consistent p (NodeEntry e)) 
                            then (deleteAndCondense e (min,max) (toDel,p))
                            else (e,[])
                        |e <- es]
deleteAndCondense ((Leaf es),pred) (min, max) (toDel, p) 
    |length newEs < min = ((Null,pred), newEs)
    |otherwise          = ((Leaf newEs, union $ map LeafEntry newEs),[])        
        where newEs = [e | e <- es, not $ consistent p (LeafEntry e)] 
        
insertGiST :: (Predicates p a) => NodeEntry p a -> (Int,Int) -> LeafEntry p a -> Level -> (NodeEntry p a, NodeEntry p a)
insertGiST (Node es,p) (min,max) (toIns,pred) l-- =(Node newEs, union $ map NodeEntry newEs) 
            |length newEs2 <= max  =  ((Node newEs2,union $ map NodeEntry (newEs2)),(Null, p) )
            |otherwise =((Node  (map unNodeEntry es1), union es1),(Node (map unNodeEntry es2), union   es2) ) 
        where   newEs =   [if (e == minSubtree)
                            then oldSub 
                            else e
                          |e <- es]
                minSubtree = chooseSubtree es (toIns,pred) 0
                (oldSub,newSub) = insertGiST minSubtree (min,max) (toIns,pred) 0
                newEs2 = if ((fst newSub) == Null) then newEs else newSub:newEs
                (es1,es2) =  pickSplit $ map NodeEntry newEs 

insertGiST (Leaf es,p) (min,max) (toIns,pred) l 
            |length newEs <= max  =  ((Leaf newEs,union $ map LeafEntry (newEs)),(Null, p) )
            |otherwise =((Leaf (map unLeafEntry es1), union  es1),(Leaf (map unLeafEntry es2), union es2) ) 
    where   newEs = ((toIns,pred) : es)
            (es1,es2) =  pickSplit $ map LeafEntry newEs 
    
insertMultiple :: (Predicates p a) => GiST p a -> (Int,Int) -> [LeafEntry p a] -> Level -> GiST p a
insertMultiple gist _ [] _ = gist
insertMultiple gist (min,max) (e:es) l = insertMultiple afterInsert (min,max) es l
    where afterInsert = insert gist (min,max) e l

-- | Chooses the most appropriate subtree to insert the entry into
chooseSubtree   :: (Predicates p a )=>[(NodeEntry p a)] -> LeafEntry p a -> Level -> (NodeEntry p a) 
chooseSubtree subtrees e l  = minPenalty penalties (head penalties)
        where penalties = [(ne, penalty e ne)|ne <- subtrees]

minPenalty :: [(NodeEntry p a, Penalty)]->(NodeEntry p a, Penalty) -> NodeEntry p a
minPenalty [] p = fst p 
minPenalty ((ne, pen):ps) (nemin, minpen) 
                                        |pen < minpen   = minPenalty ps (ne, pen) 
                                        |otherwise      = minPenalty ps (nemin, minpen) 

-- | If a node has less than the minimum number of entries, borrows entries from another node or
-- deletes the node and reinserts all its entries
condenseTree    ::(Eq a, Predicates p a) => GiST p a -> (Int, Int) -> GiST p a
condenseTree gist (min, max) = gist

makeRoot :: NodeEntry p a -> GiST p a
makeRoot (Node es, p) = Node es
makeRoot (Leaf es, p) = Leaf es

isNull :: NodeEntry p a -> Bool
isNull (Null, p) = True
isNull _ = False

getEntries :: GiST p a -> [LeafEntry p a]
getEntries (Node  es) = concat [getEntries sub | (sub,p) <- es] 
getEntries (Leaf  es) = es
