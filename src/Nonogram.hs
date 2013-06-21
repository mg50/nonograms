module Nonogram where
import Core
import System.Random
import Control.Monad
import Solver (hasUniqueSolution)

randomRow :: Int -> IO [Square]
randomRow n = do gen <- newStdGen
                 let nums = randomRs (0, 1) gen :: [Int]
                 let toSquare x = if x == 0 then Empty else Filled
                 return $ take n $ map toSquare nums

randomNonogram :: Int -> Int -> IO Nonogram
randomNonogram rows cols = do rs <- replicateM rows $ randomRow cols
                              let nono = Nonogram rs
                              if hasUniqueSolution nono
                                 then return nono
                                 else randomNonogram rows cols
