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
        putStrLn "usage : main <-b|-r> <-i key|-d key|-s interval>" 
        exitFailure
    when (length nonOpt > 0) $ do
        putStrLn "usage : main <-b|-r> <-i key|-d key|-s interval>"
        exitFailure
    if (head flags == BTree ) then do
        gist <- (load "BTreeGiST.txt" :: IO (GiST BTree.Predicate Int))
        executeOperationB gist $ head $ tail flags
    else do
        gist <- (load "RTreeGiST.txt" :: IO (GiST RTree.Predicate (Int,Int)))
        executeOperationR gist $ head $ tail flags
    
    
    
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

executeOperationB :: GiST BTree.Predicate Int -> Flag -> IO()
executeOperationB gist (Insert s)  = do     
    let key = read s :: Int
    save (insert (key, BTree.Equals key) (2,4) gist) "BTreeGiST.txt" 
executeOperationB gist (Delete s)  = do
    let key = read s :: Int 
    save (delete (key, BTree.Equals key) (2,4) gist) "BTreeGiST.txt" 
executeOperationB gist (Search s) = do
    let (min,max) = read s :: (Int,Int)
    when (min >= max) $ do
        putStrLn "usage for BTree : main <-i key|-d key|-s (min,max)>"   
        exitFailure
    putStrLn $ show (search (BTree.Contains (min,max)) gist)

executeOperationR :: GiST RTree.Predicate (Int,Int) -> Flag -> IO()
executeOperationR gist (Insert s)  = do
    let key = read s :: (Int,Int)
    save (insert (key, RTree.Equals key) (2,4) gist) "RTreeGiST.txt" 
executeOperationR gist (Delete s)  = do
    let key = read s :: (Int,Int) 
    save (delete (key, RTree.Equals key) (2,4) gist) "RTreeGiST.txt" 
executeOperationR gist (Search s) = do
    let ((minx,maxy),(maxx,miny)) = read s :: ((Int,Int),(Int,Int))
    when (minx > maxy || miny > maxy) $ do
        putStrLn "usage for RTree : main <-i key|-d key|-s`` ((minx,maxy),(maxx,miny))>"   
        exitFailure
    putStrLn $ show (search (RTree.Contains ((minx,maxy),(maxx,miny))) gist)
