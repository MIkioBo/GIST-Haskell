{-# LANGUAGE MultiParamTypeClasses
    #-}

module Test where

import Data.GiST.GiST
import Data.List(sort)
import Data.GiST.BTree

bounds = (3,5)

g1 = empty::GiST Predicate Int
g2 = insert (50, Equals 50) bounds $ insert (32, Equals 32) bounds $ insert (7, Equals 7) bounds $  insert (16, Equals 16) bounds $ insert (85, Equals 85) bounds $ insert (63, Equals 63) bounds $ insert (42, Equals 42) bounds $ insert (98, Equals 98) bounds $ insert (25, Equals 25) bounds $ insert (73, Equals 73) bounds $ insert (36, Equals 36) bounds $ insert (1, Equals 1) bounds $ insert (62, Equals 62) bounds g1
s1 = search (Contains (34,53)) g2
t1 = (sort s1) == [36, 42, 50]
s2 = search (Contains (43,82)) g2
t2 = (sort s2) == [50, 62, 63, 73]
g3 = delete (25, Equals 25) bounds $ delete (73, Equals 73) bounds $ delete (1, Equals 1) bounds g2
s3 = search (Contains (20,45)) g3
t3 = sort s3 == [32, 36, 42]
r = [t1,t2,t3]
