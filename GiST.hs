{-# LANGUAGE MultiParamTypeClasses
    ,FlexibleInstances#-}

data GiST p a  = Leaf (GiST p a) [LeafEntry p a] | Node (GiST p a) [NodeEntry p a] | Null -- | OrderedLeaf [OrderedLeafEntry a]
data Entry p a = LeafEntry (LeafEntry p a) | NodeEntry (NodeEntry p a) -- | OrderedEntry(OrderedLeafEntry a) 

type LeafEntry p a = (a, p a) 
--data OrderedLeafEntry a = OLeafEntry (OrderedLeafEntry a) (a,Predicate a) (OrderedLeafEntry a) | Nil

type NodeEntry p a = (GiST p a, p a)
--data SearchResult p a = SearchResult (GiST p a) (LeafEntry p a)
type Penalty = Integer
type Level = Integer
--data Predicate a = Predicate (a -> Bool) 

instance Eq a => Eq (LeafEntry p a) where
    (==) (a1,_) (a2,_)   = a1 == a2


class Predicates p a  where
    consistent  :: (Entry p a) -> p a -> Bool
    union       :: [(Entry p a)] -> p a
    penalty     :: (Entry p a) -> (Entry p a) -> Penalty
    pickSplit   :: [(Entry p a)] -> [[Entry p a]]

-- (Integer,Integer paare sind min und max anzahl an entries im baum)
class (Eq a, Predicates p a) => GiSTs g p a where
    search          :: g p a -> p a -> [LeafEntry p a]
    insert          :: g p a -> (Integer,Integer) -> Entry p a -> Level -> g p a
    chooseSubtree   :: g p a -> Entry p a -> Level -> g p a 
    split           :: g p a -> (Integer,Integer) -> g p a -> Entry p a -> g p a
    adjustKeys      :: g p a -> (Integer,Integer) -> g p a -> g p a
    delete          :: g p a -> (Integer,Integer) -> LeafEntry p a -> g p a 
    condenseTree    :: g p a -> (Integer,Integer) -> g p a



--delete2 g (min, max) e1 ((SearchResult (Leaf par es) e2):results) 
--deleteFromNode :: (Eq a, Predicates p a) => (Integer,Integer) -> LeafEntry p a -> [SearchResult p a] -> g p a
--deleteFromNode g _ _ []      = g
--deleteFromNode g (min, max) e1 ((SearchResult (Leaf par es) e2):results) 
--        |e1 == e2       = condenseTree g (min,max) (Leaf par (filter (/=e2) es))


instance (Eq a, Predicates p a) => GiSTs GiST p a where
    search (Leaf par es) p          = [e | e <- es, consistent (LeafEntry e) p] 
    search (Node _ []) _            = []
    search (Node par (e:es)) p
        |consistent (NodeEntry e) p = (search (fst e) p) ++ (search (Node par es) p)
        |otherwise                  = search (Node par es) p
    insert gist (min,max) e l   = gist 
    chooseSubtree gist e l = gist 
    split gist (min,max) gist2  e = gist2 
    adjustKeys gist (min,max) gist2 = gist2 
    
    --delete (Node par []) _ _ = 
    delete (Node par (es)) (min, max) (a, p) = condenseTree afterDelete (min, max)
        where afterDelete = (Node par [if (consistent (NodeEntry (subTree, p1)) p) 
                                            then ((delete subTree (min, max) (a ,p)), p1) 
                                            else (subTree, p1) 
                                        | (subTree, p1) <- es])
        
    condenseTree node (min,max) = node
    
    --delete g (min, max) e   =  condenseTree g (min,max) delete
    
     --class Predicates p a where
--	consistent :: (Entry a) -> Predicate a-> Bool
--	union :: [(Entry a)] -> Predicate a
--	penalty :: (Entry a) -> (Entry a) -> a
--
--type Predicate a = (a -> Bool)
--type Node = [(Entry a)]
--
--  
--data Entry a = Entry (Predicate a)  Node	

--class OrderedGiSTs g a where
--    findMin  ::  g a -> Predicate a -> OrderedLeafEntry a
--    next    ::  g a -> GiST a       -> OrderedLeafEntry a



{--instance (Eq a) => Predicates Predicate a where
    consistent e p = True
    union ((LeafEntry (a1,p)):es) = p
    union ((NodeEntry (g, p)):es) = p
    penalty e1 e2 =  0
    pickSplit  (e:es) = [es]
--}

