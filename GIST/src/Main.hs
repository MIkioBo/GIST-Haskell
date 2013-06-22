{-# LANGUAGE MultiParamTypeClasses
    #-}

module Main where

import System.IO
import System.Exit
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
    when (length flags /=2) $ do 
        putStrLn "usage : main <-b|-r> <-i key|-d key|-s interval> file" 
        exitFailure
    when (length nonOpt /= 1) $ do
        putStrLn "usage : main <-b|-r> <-i key|-d key|-s interval> file"
        exitFailure
    if (head flags == BTree ) then do
        gist <- (load $ nonOpt!!0) :: IO (GiST BTree.Predicate Int)
        executeOperationB gist (flags!!1) $ nonOpt!!0
    else do
        gist <- (load $ nonOpt!!0) :: IO (GiST RTree.Predicate (Int,Int))
        executeOperationR gist (flags!!1) $ nonOpt!!0
    
    
    
data Flag = BTree | RTree | Search String | Insert String | Delete String deriving (Eq)

options :: [OptDescr Flag]
options = [
    Option ['b'] ["btree"] (NoArg BTree)  "use BTree GiST",
    Option ['r'] ["rtree"] (NoArg RTree)  "use RTree GiST",
    Option ['s'] ["search"] (ReqArg Search  "DATA")  "search in GiST",
    Option ['i'] ["insert"] (ReqArg Insert  "DATA") "insert into GiST",
    Option ['d'] ["delete"] (ReqArg Delete  "DATA") "delete from GiST"
  ]
   
    
    
    {--outh <- openFile "GiST.txt" WriteMode
    hPutStrLn outh $ show $ (empty :: GiST BTreePredicate Int) 
    hClose outh--}

load :: (Read a) => FilePath -> IO a
load f = do s <- TIO.readFile f
            return (read $ T.unpack s)

save :: (Show a) => a -> FilePath -> IO ()
save x f = TIO.writeFile f $ T.pack (show x)

executeOperationB :: GiST BTree.Predicate Int -> Flag -> String -> IO()
executeOperationB gist (Insert s) file  = do     
    let key = read s :: Int
    save (insert (key, BTree.Equals key) (2,4) gist) file
executeOperationB gist (Delete s) file = do
    let key = read s :: Int 
    save (delete (key, BTree.Equals key) (2,4) gist) file 
executeOperationB gist (Search s) file = do
    let (min,max) = read s :: (Int,Int)
    when (min >= max) $ do
        putStrLn "usage for BTree : main <-i key|-d key|-s (min,max)>"   
        exitFailure
    putStrLn $ show (search (BTree.Contains (min,max)) gist)

executeOperationR :: GiST RTree.Predicate (Int,Int) -> Flag -> String -> IO()
executeOperationR gist (Insert s) file  = do
    let key = read s :: (Int,Int)
    save (insert (key, RTree.Equals key) (2,4) gist) file 
executeOperationR gist (Delete s) file = do
    let key = read s :: (Int,Int) 
    save (delete (key, RTree.Equals key) (2,4) gist) file 
executeOperationR gist (Search s) file = do
    let ((minx,maxy),(maxx,miny)) = read s :: ((Int,Int),(Int,Int))
    when (minx > maxy || miny > maxy) $ do
        putStrLn "usage for RTree : main <-i key|-d key|-s`` ((minx,maxy),(maxx,miny))>"   
        exitFailure
    putStrLn $ show (search (RTree.Contains ((minx,maxy),(maxx,miny))) gist)
