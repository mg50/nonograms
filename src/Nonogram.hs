module Nonogram where
import Core
import System.Random
import Control.Monad
import Solver (hasUniqueSolution)
import Data.Time.Clock
import System.IO (hFlush, stdout)
import System.Timeout

randomRow :: Int -> IO [Square]
randomRow n = do gen <- newStdGen
                 let nums = randomRs (0, 1) gen :: [Int]
                 let toSquare x = if x == 0 then Empty else Filled
                 return $ take n $ map toSquare nums

uniqueMessage :: (Show a) => a -> Bool -> String
uniqueMessage time unique = "took " ++ (show time) ++ " to deduce" ++ rest
  where rest = if unique then " uniquely solvable" else " multiple solutions"

randomNonogram :: Int -> Int -> IO Nonogram
randomNonogram rows cols = do putStr "Generating new nonogram... "
                              hFlush stdout
                              rs <- replicateM rows $ randomRow cols
                              start <- getCurrentTime
                              let nono = Nonogram rs
                                  unique = hasUniqueSolution nono
                              endTime <- timeout 2000000 $ unique `seq` getCurrentTime
                              case endTime of
                                Nothing -> do putStrLn "timed out looking for multiple solutions"
                                              randomNonogram rows cols
                                Just end -> do putStrLn $ uniqueMessage (diffUTCTime end start) unique
                                               if unique
                                                  then return nono
                                                  else randomNonogram rows cols
