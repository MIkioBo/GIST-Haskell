{-# LANGUAGE MultiParamTypeClasses
    #-}

module RandomFast where

import System.IO
import System.Exit
import System.Random
import Data.GiST.GiST
import qualified Data.GiST.BTree as BTree
import qualified Data.GiST.RTree as RTree
import Control.Monad(when)
import System.Environment(getArgs)
import System.Console.GetOpt
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

main    :: IO()
main =  do
    args <- getArgs  
    let (flags, nonOpt, msgs) = getOpt RequireOrder options args
    when (length flags /=1) $ do 
        putStrLn "usage : main <-b|-r> max_range num_inserts" 
        exitFailure
    when (length nonOpt /= 2) $ do
        putStrLn "usage : main <-b|-r> max_range num_inserts"
        exitFailure
    if (head flags == BTree ) then do
        let gist = empty :: GiST BTree.Predicate Int
        g <- newStdGen
        let file = "BTreeRandom.txt"
        save (executeOperationB gist (read $ nonOpt!!0) (read $ nonOpt!!1) g) file
        
    else do
        let gist = empty :: GiST RTree.Predicate (Int,Int)
        g <- newStdGen
        let file = "RTreeRandom.txt"
        save (executeOperationR gist (read $ nonOpt!!0) (read $ nonOpt!!1) g) file
        
    
    
    
data Flag = BTree | RTree deriving (Eq)

options :: [OptDescr Flag]
options = [
    Option ['b'] ["btree"] (NoArg BTree)  "use BTree GiST",
    Option ['r'] ["rtree"] (NoArg RTree)  "use RTree GiST"
  ]
   
    
    
    {--outh <- openFile "GiST.txt" WriteMode
    hPutStrLn outh $ show $ (empty :: GiST BTreePredicate Int) 
    hClose outh--}

load :: (Read a) => FilePath -> IO a
load f = do s <- TIO.readFile f
            return (read $ T.unpack s)

save :: (Show a) => a -> FilePath -> IO ()
save x f = TIO.writeFile f $ T.pack (show x)

executeOperationB :: GiST BTree.Predicate Int -> Int -> Int -> StdGen -> GiST BTree.Predicate Int
executeOperationB g _ 0 _ = g
executeOperationB gist max num gen = executeOperationB inserted max (num-1) g
    where   inserted = insert (key, BTree.Equals key) (3,6) gist
            (key,g) = randomR (1,max) gen

executeOperationR :: GiST RTree.Predicate (Int,Int) -> Int -> Int -> StdGen -> GiST RTree.Predicate (Int,Int)
executeOperationR g _ 0 _ = g
executeOperationR gist max num gen = executeOperationR inserted max (num-1) g2
    where   inserted = insert ((x,y), RTree.Equals (x,y)) (3,6) gist
            (x,g) = randomR (1,max) gen
            (y,g2) = randomR (1,max) g
{--           
            do
    gist <- (load file :: IO (GiST RTree.Predicate (Int,Int)))
    let (x,g) = randomR (1,max) gen
    let (y,g2) = randomR (1,max) g
    putStrLn $ show $ length $ getEntries gist
    --putStrLn $ show (x,y)
    --putStrLn $ show gist
    save (insert ((x,y), RTree.Equals (x,y)) (3,6) gist) file
    executeOperationR file max (num-1) g2 --}
