{-# LANGUAGE MultiParamTypeClasses
    #-}

module Main where

import System.IO
import System.Exit
import Data.GiST.GiST
import Data.List(sort)
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
        putStrLn "no options recognised" 
        exitFailure
    putStrLn "lala" 
    let pred = BTP (read (head nonOpt) :: (Int,Int))
    gist <- (load "GiST.txt" :: IO (GiST BTreePredicate Int))
    putStrLn "lala" 
    executeOperation gist pred $ head flags  
    
    
    
data Flag = Search | Insert String | Delete String

options :: [OptDescr Flag]
options = [
    Option ['s'] ["search"] (NoArg Search)  "search in GiST",
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
save x f = writeFile f (show x)

executeOperation :: GiST BTreePredicate Int -> BTreePredicate Int -> Flag -> IO()
executeOperation gist p (Insert s)  = do
    file <- openFile "Gist.txt" WriteMode 
    putStrLn "lala"    
    save (insert gist (2,4) (read s, p)) "GiST.txt"    
    putStrLn "lala" 
    hClose file
executeOperation gist p (Delete s)  = do
    file <- openFile "Gist.txt" WriteMode 
    save (delete gist (2,4) (read s, p)) "GiST.txt" 
    hClose file
executeOperation gist p Search      = putStrLn $ show (search gist p)

newtype BTreePredicate a = BTP (a,a) deriving (Eq, Ord, Show, Read)

unBTP (BTP a) = a

-- | Checks if the intervals of two BTreePredicates overlap
overlaps :: (Ord a) => BTreePredicate a -> BTreePredicate a -> Bool
overlaps (BTP (min1, max1)) (BTP (min2, max2)) = (min1>=min2 && min1<=max2) || (max1>=min2 && max1<=max2)  



instance Predicates BTreePredicate Int where
    
    consistent p1 (NodeEntry (_, p2)) = overlaps p1 p2 
    consistent p1 (LeafEntry (_, p2)) = overlaps p1 p2
    
    union es = BTP (min,max)
        where   predicates  = map unBTP $ map entryPredicate es
                -- | The minimum of all interval minimums
                min         = minimum $ map fst predicates
                -- | The maximum of all interval maximums
                max         = maximum $ map snd predicates
    
    -- | Seperates the sorted list of entries into two halves
    pickSplit es =  (take ((length es) `div` 2) $ sort es, drop ((length es) `div` 2) $ sort es) 

    penalty (_, BTP (min1,max1)) (_, BTP(min2,max2)) = maximum[min2-min1, 0] + maximum [max1-max2,0]

