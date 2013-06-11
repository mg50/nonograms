module Nonogram where
import Core
import System.Random
import Control.Monad

randomRow :: Int -> IO [Square]
randomRow n = do gen <- newStdGen
                 let nums = randomRs (0, 1) gen :: [Int]
                 let toSquare x = if x == 0 then Empty else Filled
                 return $ take n $ map toSquare nums

randomNonogram :: Int -> Int -> IO Nonogram
randomNonogram rows cols = do rs <- replicateM rows $ randomRow cols
                              return $ Nonogram rs
  -- gen <- newStdGen
  -- let nums = randomRs (0, 1) gen :: [Int]
  -- let toSquare x = if x == 0 then Empty else Filled


  -- let squares = take (rows * cols) $ map toSquare nums
  -- return $ Nonogram squares cols
  -- -- squares <- sequence (liftM toSquare) ns
  -- -- undefined
