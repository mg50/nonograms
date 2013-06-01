module Nonogram where
import Core
import System.Random
import Control.Monad

randomNonogram :: Int -> Int -> IO Nonogram
randomNonogram rows cols = do
  gen <- newStdGen
  let nums = randomRs (0, 1) gen :: [Int]
  let toSquare x = if x == 0 then Empty else Filled
  let squares = take (rows * cols) $ map toSquare nums
  return $ Nonogram squares cols
  -- squares <- sequence (liftM toSquare) ns
  -- undefined
