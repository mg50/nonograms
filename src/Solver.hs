{-# LANGUAGE BangPatterns #-}

module Solver where
import qualified Data.List as L
import Core (Nonogram, hintsForNonogram)
import Data.ZDD (ZDD(..), union, intersection, disjoin, toList)
import Control.Parallel

contiguous :: Int -> [Int] -> ZDD
contiguous n row
  | n > length row = Bottom
  | n == 0 = Top
  | otherwise =  case row of
                   [] -> Top
                   (r:rs) -> let hi = contiguous (n-1) rs
                                 lo = Bottom
                             in Node r hi lo

rowZdd :: [Int] -> [Int] -> ZDD
rowZdd hints [] = Top
rowZdd [] row = Top
rowZdd hints row = foldr1 union $ map (go hints) (L.tails row)
  where go hints row
          | score hints > length row = Bottom
          | otherwise = let h:hs = hints
                            cont = contiguous h row
                        in cont `disjoin` rowZdd hs (drop (h + 1) row)

        score [] = 0
        score xs = sum xs + length xs - 1

for = flip map

zdd :: [[Int]] -> [[Int]] -> ZDD
zdd rowHints colHints =
  let numRows = length rowHints
      numCols = length colHints

      rowZdds = for (rowHints `zip` [0..]) $ \(rowHint, rowNum) ->
        let row = take numCols [rowNum*numCols..] in rowZdd rowHint row
      colZdds = for (colHints `zip` [0..]) $ \(colHint, colNum) ->
        let col = take numRows [colNum,colNum+numCols..] in rowZdd colHint col
      merge = foldr1 disjoin

      zddForAllRows = merge rowZdds
      zddForAllCols = merge colZdds
  in zddForAllRows `par` zddForAllCols `pseq` (zddForAllCols `intersection` zddForAllRows)

hasUniqueSolution :: Nonogram -> Bool
hasUniqueSolution nono =
  let hints = hintsForNonogram nono
      nonoZdd = uncurry zdd hints
  in case toList nonoZdd of
    _:[] -> True
    _    -> False
