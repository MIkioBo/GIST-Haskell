{-# LANGUAGE MultiParamTypeClasses
    ,FlexibleInstances#-}

data GiST p a  = Leaf (GiST p a) [LeafEntry p a] | Node (GiST p a) [NodeEntry p a] | Null deriving (Eq, Show) -- | OrderedLeaf [OrderedLeafEntry a]
data Entry p a = LeafEntry (LeafEntry p a) | NodeEntry (NodeEntry p a) deriving (Eq, Show) -- | OrderedEntry(OrderedLeafEntry a) 

type LeafEntry p a = (a, p a) 
--data OrderedLeafEntry a = OLeafEntry (OrderedLeafEntry a) (a,Predicate a) (OrderedLeafEntry a) | Nil

type NodeEntry p a = (GiST p a, p a)

type Penalty = Int
type Level = Int
--data Predicate a = Predicate (a -> Bool) 



class Predicates p a  where
    -- | Checks if the given entry is consistent with a given predicate
    consistent  :: p a -> Entry p a -> Bool
    -- | Returns a predicate that is the union of all predicates of the given list of entries
    union       :: [(Entry p a)] -> p a
    -- | Calculates a numerical penalty for inserting the first entry into a subtree rooted int he first
    penalty     :: (Entry p a) -> (Entry p a) -> Penalty
    -- | Given a list of entries, returns two disjunct subsets that contain the entries in the list
    -- Focus is on minimising the fill factor
    pickSplit   :: [(Entry p a)] -> [[Entry p a]]

class (Eq a, Predicates p a) => GiSTs g p a where
    -- | Searches the GiST for leaf nodes that satisfy the given search predicate
    search          :: g p a -> p a -> [LeafEntry p a]
    -- | Inserts an entry into the tree, rebalancing the tree if necessary
    insert          :: g p a -> (Int, Int) -> LeafEntry p a -> Level -> g p a
       -- | Deletes a leaf entry from the tree and rebalances if necessary 
    delete          :: g p a -> (Int, Int) -> LeafEntry p a -> g p a 
    


instance (Eq a, Predicates p a) => GiSTs GiST p a where

    search (Leaf par es) p          = [e | e <- es, consistent p (LeafEntry e)] 
    search (Node _ []) _            = []
    search (Node par (e:es)) p
        |consistent p (NodeEntry e) = (search (fst e) p) ++ (search (Node par es) p)
        |otherwise                  = search (Node par es) p
  
    insert = insertGiST
    
    delete (Node Null es) (min,max) (toDel, p)
            |length newEs == 1  = insertMultiple (makeRoot $ head newEs) (min,max) toAdd 0
            |otherwise          = insertMultiple (Node Null newEs) (min, max) toAdd 0
        where   newEs = filter (not.isNull) (map fst delNodes)  
                toAdd = concat (map snd delNodes)
                delNodes =  [if (consistent p (NodeEntry e)) 
                                then (deleteAndCondense e (min,max) (toDel,p))
                                else (e,[])
                            |e <- es]
    delete (Leaf Null es) (min,max) (toDel,p) = Leaf Null [e | e <- es, not $ consistent p (LeafEntry e)]
        
   
{--instance (Eq a) => Predicates Predicate a where
    consistent e p = True
    union ((LeafEntry (a1,p)):es) = p
    union ((NodeEntry (g, p)):es) = p
    penalty e1 e2 =  0
    pickSplit  (e:es) = [es]
--}

deleteAndCondense :: (Eq a, Predicates p a) => NodeEntry p a -> (Int,Int) -> LeafEntry p a -> (NodeEntry p a, [LeafEntry p a])
deleteAndCondense (Node par es, pred) (min, max) (toDel, p)
        |length newEs < min = ((Null, pred), toAdd ++ getEntries (Node par es))   
        |otherwise          = ((Node par newEs, union $ map NodeEntry newEs), toAdd)
    where   newEs = filter (not.isNull) (map fst delNodes)  
            toAdd = concat (map snd delNodes)
            delNodes =  [if (consistent p (NodeEntry e)) 
                            then (deleteAndCondense e (min,max) (toDel,p))
                            else (e,[])
                        |e <- es]
deleteAndCondense ((Leaf par es),pred) (min, max) (toDel, p) 
    |length newEs < min = ((Null,pred), newEs)
    |otherwise          = ((Leaf par newEs, union $ map LeafEntry newEs),[])        
        where newEs = [e | e <- es, not $ consistent p (LeafEntry e)] 
        
insertGiST :: (Eq a,Predicates p a) => GiST p a -> (Int,Int) -> LeafEntry p a -> Level -> GiST p a
insertGiST gist (min,max) e l = gist

insertMultiple :: (Eq a,Predicates p a) => GiST p a -> (Int,Int) -> [LeafEntry p a] -> Level -> GiST p a
insertMultiple gist _ [] _ = gist
insertMultiple gist (min,max) (e:es) l = insertMultiple afterInsert (min,max) es l
    where afterInsert = insert gist (min,max) e l

-- | Chooses the most appropriate subtree to insert the entry into
chooseSubtree   :: g p a -> Entry p a -> Level -> g p a 
chooseSubtree gist e l = gist
-- | Splits the tree in two subtrees if the maximum number of entries es exceeded
split           :: g p a -> (Int, Int) -> g p a -> Entry p a -> g p a
split gist (min, max) n e = gist
-- | Makes sure the predicate of an entry is the union of all entry predicates of the subtree it points to
adjustKeys      :: g p a -> (Int, Int) -> g p a -> g p a
adjustKeys gist (min,max) n = gist
-- | If a node has less than the minimum number of entries, borrows entries from another node or
-- deletes the node and reinserts all its entries
condenseTree    ::(Eq a, Predicates p a) => GiST p a -> (Int, Int) -> GiST p a
condenseTree gist (min, max) = gist

makeRoot :: NodeEntry p a -> GiST p a
makeRoot (Node par es, p) = Node Null es
makeRoot (Leaf par es, p) = Leaf Null es

isNull :: NodeEntry p a -> Bool
isNull (Null, p) = True
isNull _ = False

getEntries :: GiST p a -> [LeafEntry p a]
getEntries (Node _ es) = concat [getEntries sub | (sub,p) <- es] 
getEntries (Leaf _ es) = es
