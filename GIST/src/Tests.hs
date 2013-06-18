{-# LANGUAGE MultiParamTypeClasses
    #-}

module Test where

import Data.GiST.GiST
import Data.List(sort)
import Data.GiST.Predicates

bounds = (3,5)

g1 = empty::GiST BTreePredicate Int
g2 = insert (50,BTP (50,50)) bounds $ insert (32,BTP (32,32)) bounds $ insert (7,BTP (7,7)) bounds $  insert (16,BTP (16,16)) bounds $ insert (85,BTP (85,85)) bounds $ insert (63,BTP (63,63)) bounds $ insert (42,BTP (42,42)) bounds $ insert (98,BTP (98,98)) bounds $ insert (25,BTP (25,25)) bounds $ insert (73,BTP (73,73)) bounds $ insert (36,BTP (36,36)) bounds $ insert (1,BTP (1,1)) bounds $ insert (62,BTP (62,62)) bounds g1
s1 = search (BTP (34,53)) g2
t1 = (sort s1) == [(36,BTP (36,36)),(42,BTP (42,42)),(50,BTP (50,50))]
s2 = search (BTP (43,82)) g2
t2 = (sort s2) == [(50,BTP (50,50)),(62,BTP (62,62)),(63,BTP (63,63)), (73,BTP (73,73))]
g3 = delete (25, BTP (25,25)) bounds $ delete (73, BTP (73,73)) bounds $ delete (1,BTP (1,1)) bounds g2
s3 = search (BTP (20,45)) g3
t3 = sort s3 == [(32,BTP (32,32)),(36,BTP (36,36)),(42,BTP (42,42))]
r = [t1,t2,t3]
