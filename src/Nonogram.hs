module Nonogram where
import Core
import System.Random
import Control.Monad
import Solver (hasUniqueSolution)
import Data.Time.Clock

randomRow :: Int -> IO [Square]
randomRow n = do gen <- newStdGen
                 let nums = randomRs (0, 1) gen :: [Int]
                 let toSquare x = if x == 0 then Empty else Filled
                 return $ take n $ map toSquare nums

randomNonogram :: Int -> Int -> IO Nonogram
randomNonogram rows cols = do putStr "Generating new nonogram... "
                              rs <- replicateM rows $ randomRow cols
                              startTime <- getCurrentTime
                              let nono = Nonogram rs
                                  unique = hasUniqueSolution nono
                              endTime <- unique `seq` getCurrentTime
                              putStrLn $ "took " ++ show (diffUTCTime endTime startTime) ++ " to solve"
                              if unique
                                 then return nono
                                 else randomNonogram rows cols
