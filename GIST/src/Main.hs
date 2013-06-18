{-# LANGUAGE MultiParamTypeClasses
    #-}

module Main where

import System.IO
import System.Exit
import Data.GiST.GiST
import Data.GiST.Predicates
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
    let pred = BTP (read (head nonOpt) :: (Int,Int))
    gist <- (load "GiST.txt" :: IO (GiST BTreePredicate Int))
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
    save (insert (read s, p) (2,4) gist) "GiST.txt" 
    hClose file
executeOperation gist p (Delete s)  = do
    file <- openFile "Gist.txt" WriteMode 
    save (delete (read s, p) (2,4) gist) "GiST.txt" 
    hClose file
executeOperation gist p Search      = putStrLn $ show (search p gist)


